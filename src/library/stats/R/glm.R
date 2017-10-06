#  File src/library/stats/R/glm.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

utils::globalVariables("n", add = TRUE)

### This function fits a generalized linear model via
### iteratively reweighted least squares for any family.
### Written by Simon Davies, Dec 1995
### glm.fit modified by Thomas Lumley, Apr 1997, and then others..

glm <- function(formula, family = gaussian, data, weights,
		subset, na.action, start = NULL,
		etastart, mustart, offset,
		control = list(...),
                model = TRUE, method = "glm.fit",
                x = FALSE, y = TRUE,
		singular.ok = TRUE, contrasts = NULL, ...)
{
    call <- match.call()
    ## family
    if(is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if(is.function(family)) family <- family()
    if(is.null(family$family)) {
	print(family)
	stop("'family' not recognized")
    }

    ## extract x, y, etc from the model formula and frame
    if(missing(data)) data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    ## need stats:: for non-standard evaluation
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if(identical(method, "model.frame")) return(mf)

    if (!is.character(method) && !is.function(method))
        stop("invalid 'method' argument")
    ## for back-compatibility in return result
    if (identical(method, "glm.fit"))
        control <- do.call("glm.control", control)

    mt <- attr(mf, "terms") # allow model.frame to have updated it

    Y <- model.response(mf, "any") # e.g. factors are allowed
    ## avoid problems with 1D arrays, but keep names
    if(length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if(!is.null(nm)) names(Y) <- nm
    }
    ## null model support
    X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts) else matrix(,NROW(Y), 0L)
    ## avoid any problems with 1D or nx1 arrays by as.vector.
    weights <- as.vector(model.weights(mf))
    if(!is.null(weights) && !is.numeric(weights))
        stop("'weights' must be a numeric vector")
    ## check weights and offset
    if( !is.null(weights) && any(weights < 0) )
	stop("negative weights not allowed")

    offset <- as.vector(model.offset(mf))
    if(!is.null(offset)) {
        if(length(offset) != NROW(Y))
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", length(offset), NROW(Y)), domain = NA)
    }
    ## these allow starting values to be expressed in terms of other vars.
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")

    ## We want to set the name on this call and the one below for the
    ## sake of messages from the fitter function
    fit <- eval(call(if(is.function(method)) "method" else method,
                     x = X, y = Y, weights = weights, start = start,
                     etastart = etastart, mustart = mustart,
                     offset = offset, family = family, control = control,
                     intercept = attr(mt, "intercept") > 0L, singular.ok = singular.ok))

    ## This calculated the null deviance from the intercept-only model
    ## if there is one, otherwise from the offset-only model.
    ## We need to recalculate by a proper fit if there is intercept and
    ## offset.
    ##
    ## The glm.fit calculation could be wrong if the link depends on the
    ## observations, so we allow the null deviance to be forced to be
    ## re-calculated by setting an offset (provided there is an intercept).
    ## Prior to 2.4.0 this was only done for non-zero offsets.
    if(length(offset) && attr(mt, "intercept") > 0L) {
        fit2 <-
            eval(call(if(is.function(method)) "method" else method,
                      x = X[, "(Intercept)", drop=FALSE], y = Y,
                      weights = weights, offset = offset, family = family,
                      control = control, intercept = TRUE))
        ## That fit might not have converged ....
        if(!fit2$converged)
            warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
        fit$null.deviance <- fit2$deviance
    }
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if(x) fit$x <- X
    if(!y) fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula,
		       terms = mt, data = data,
		       offset = offset, control = control, method = method,
		       contrasts = attr(X, "contrasts"),
                       xlevels = .getXlevels(mt, mf)))
    class(fit) <- c(fit$class, c("glm", "lm"))
    fit
}


glm.control <- function(epsilon = 1e-8, maxit = 25, trace = FALSE)
{
    if(!is.numeric(epsilon) || epsilon <= 0)
	stop("value of 'epsilon' must be > 0")
    if(!is.numeric(maxit) || maxit <= 0)
	stop("maximum number of iterations must be > 0")
    list(epsilon = epsilon, maxit = maxit, trace = trace)
}

## Modified by Thomas Lumley 26 Apr 97
## Added boundary checks and step halving
## Modified detection of fitted 0/1 in binomial
## Updated by KH as suggested by BDR on 1998/06/16

glm.fit <-
    function (x, y, weights = rep(1, nobs), start = NULL,
	      etastart = NULL, mustart = NULL, offset = rep(0, nobs),
	      family = gaussian(), control = list(), intercept = TRUE,
	      singular.ok = TRUE)
{
    control <- do.call("glm.control", control)
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2L]]
    ynames <- if(is.matrix(y)) rownames(y) else names(y)
    conv <- FALSE
    nobs <- NROW(y)
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    ## define weights and offset if needed
    if (is.null(weights))
	weights <- rep.int(1, nobs)
    if (is.null(offset))
	offset <- rep.int(0, nobs)

    ## get family functions:
    variance <- family$variance
    linkinv  <- family$linkinv
    if (!is.function(variance) || !is.function(linkinv) )
	stop("'family' argument seems not to be a valid family object", call. = FALSE)
    dev.resids <- family$dev.resids
    aic <- family$aic
    mu.eta <- family$mu.eta
    unless.null <- function(x, if.null) if(is.null(x)) if.null else x
    valideta <- unless.null(family$valideta, function(eta) TRUE)
    validmu  <- unless.null(family$validmu,  function(mu) TRUE)
    if(is.null(mustart)) {
        ## calculates mustart and may change y and weights and set n (!)
        eval(family$initialize)
    } else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if(EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta))
            stop("invalid linear predictor values in empty model", call. = FALSE)
        mu <- linkinv(eta)
        ## calculate initial deviance and coefficient
        if (!validmu(mu))
            stop("invalid fitted means in empty model", call. = FALSE)
        dev <- sum(dev.resids(y, mu, weights))
        w <- sqrt((weights * mu.eta(eta)^2)/variance(mu))
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep_len(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric()
        iter <- 0L
    } else {
        coefold <- NULL
        eta <-
            if(!is.null(etastart)) etastart
            else if(!is.null(start))
                if (length(start) != nvars)
                    stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", nvars, paste(deparse(xnames), collapse=", ")),
                         domain = NA)
                else {
                    coefold <- start
                    offset + as.vector(if (NCOL(x) == 1L) x * start else x %*% start)
                }
            else family$linkfun(mustart)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta)))
            stop("cannot find valid starting values: please specify some", call. = FALSE)
        ## calculate initial deviance and coefficient
        devold <- sum(dev.resids(y, mu, weights))
        boundary <- conv <- FALSE

        ##------------- THE Iteratively Reweighting L.S. iteration -----------
        for (iter in 1L:control$maxit) {
            good <- weights > 0
            varmu <- variance(mu)[good]
            if (anyNA(varmu))
                stop("NAs in V(mu)")
            if (any(varmu == 0))
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good])))
                stop("NAs in d(mu)/d(eta)")
            ## drop observations for which w will be zero
            good <- (weights > 0) & (mu.eta.val != 0)

            if (all(!good)) {
                conv <- FALSE
                warning(gettextf("no observations informative at iteration %d",
                                 iter), domain = NA)
                break
            }
            z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
            w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
            ## call Fortran code via C wrapper
            fit <- .Call(C_Cdqrls, x[good, , drop = FALSE] * w, z * w,
                         min(1e-7, control$epsilon/1000), check=FALSE)
            if (any(!is.finite(fit$coefficients))) {
                conv <- FALSE
                warning(gettextf("non-finite coefficients at iteration %d", iter), domain = NA)
                break
            }
            ## stop if not enough parameters
            if (nobs < fit$rank)
                stop(sprintf(ngettext(nobs,
                                      "X matrix has rank %d, but only %d observation",
                                      "X matrix has rank %d, but only %d observations"),
                             fit$rank, nobs), domain = NA)
            if(!singular.ok && fit$rank < nvars) stop("singular fit encountered")
            ## calculate updated values of eta and mu with the new coef:
            start[fit$pivot] <- fit$coefficients
            eta <- drop(x %*% start)
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace)
                cat("Deviance = ", dev, " Iterations - ", iter, "\n", sep = "")
            ## check for divergence
            boundary <- FALSE
            if (!is.finite(dev)) {
                if(is.null(coefold))
                    stop("no valid set of coefficients has been found: please supply starting values", call. = FALSE)
                warning("step size truncated due to divergence", call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                    if (ii > control$maxit)
                        stop("inner loop 1; cannot correct step size", call. = FALSE)
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                    dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace)
                    cat("Step halved: new deviance = ", dev, "\n", sep = "")
            }
            ## check for fitted values outside domain.
            if (!(valideta(eta) && validmu(mu))) {
                if(is.null(coefold))
                    stop("no valid set of coefficients has been found: please supply starting values", call. = FALSE)
                warning("step size truncated: out of bounds", call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                    if (ii > control$maxit)
                        stop("inner loop 2; cannot correct step size", call. = FALSE)
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace)
                    cat("Step halved: new deviance = ", dev, "\n", sep = "")
            }
            ## check for convergence
            if (abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
                conv <- TRUE
                coef <- start
                break
            } else {
                devold <- dev
                coef <- coefold <- start
            }
        } ##-------------- end IRLS iteration -------------------------------

        if (!conv)
            warning("glm.fit: algorithm did not converge", call. = FALSE)
        if (boundary)
            warning("glm.fit: algorithm stopped at boundary value", call. = FALSE)
        eps <- 10*.Machine$double.eps
        if (family$family == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps))
                warning("glm.fit: fitted probabilities numerically 0 or 1 occurred", call. = FALSE)
        }
        if (family$family == "poisson") {
            if (any(mu < eps))
                warning("glm.fit: fitted rates numerically 0 occurred", call. = FALSE)
        }
        ## If X matrix was not full rank then columns were pivoted,
        ## hence we need to re-label the names ...
        ## Original code changed as suggested by BDR---give NA rather
        ## than 0 for non-estimable parameters
        if (fit$rank < nvars) coef[fit$pivot][seq.int(fit$rank+1, nvars)] <- NA
        xxnames <- xnames[fit$pivot]
        ## update by accurate calculation, including 0-weight cases.
        residuals <-  (y - mu)/mu.eta(eta)
##        residuals <- rep.int(NA, nobs)
##        residuals[good] <- z - (eta - offset)[good] # z does not have offset in.
        fit$qr <- as.matrix(fit$qr)
        nr <- min(sum(good), nvars)
        if (nr < nvars) {
            Rmat <- diag(nvars)
            Rmat[1L:nr, 1L:nvars] <- fit$qr[1L:nr, 1L:nvars]
        }
        else Rmat <- fit$qr[1L:nvars, 1L:nvars]
        Rmat <- as.matrix(Rmat)
        Rmat[row(Rmat) > col(Rmat)] <- 0
        names(coef) <- xnames
        colnames(fit$qr) <- xxnames
        dimnames(Rmat) <- list(xxnames, xxnames)
    }
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    # for compatibility with lm, which has a full-length weights vector
    wt <- rep.int(0, nobs)
    wt[good] <- w^2
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    if(!EMPTY)
        names(fit$effects) <-
            c(xxnames[seq_len(fit$rank)], rep.int("", sum(good) - fit$rank))
    ## calculate null deviance -- corrected in glm() if offset and intercept
    wtdmu <-
	if (intercept) sum(weights * y)/sum(weights) else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    ## calculate df
    n.ok <- nobs - sum(weights==0)
    nulldf <- n.ok - as.integer(intercept)
    rank <- if(EMPTY) 0 else fit$rank
    resdf  <- n.ok - rank
    ## calculate AIC
    aic.model <-
	aic(y, n, mu, weights, dev) + 2*rank
	##     ^^ is only initialize()d for "binomial" [yuck!]
    list(coefficients = coef, residuals = residuals, fitted.values = mu,
	 effects = if(!EMPTY) fit$effects, R = if(!EMPTY) Rmat, rank = rank,
	 qr = if(!EMPTY) structure(fit[c("qr", "rank", "qraux", "pivot", "tol")], class = "qr"),
         family = family,
	 linear.predictors = eta, deviance = dev, aic = aic.model,
	 null.deviance = nulldev, iter = iter, weights = wt,
	 prior.weights = weights, df.residual = resdf, df.null = nulldf,
	 y = y, converged = conv, boundary = boundary)
}


print.glm <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("\nCall:  ",
	paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if(length(coef(x))) {
        cat("Coefficients")
        if(is.character(co <- x$contrasts))
            cat("  [contrasts: ",
                apply(cbind(names(co),co), 1L, paste, collapse = "="), "]")
        cat(":\n")
        print.default(format(x$coefficients, digits = digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
    cat("Null Deviance:	   ",	format(signif(x$null.deviance, digits)),
	"\nResidual Deviance:", format(signif(x$deviance, digits)),
	"\tAIC:", format(signif(x$aic, digits)))
    cat("\n")
    invisible(x)
}


anova.glm <- function(object, ..., dispersion = NULL, test = NULL)
{
    ## check for multiple objects
    dotargs <- list(...)
    named <- if (is.null(names(dotargs)))
	rep_len(FALSE, length(dotargs)) else (names(dotargs) != "")
    if(any(named))
	warning("the following arguments to 'anova.glm' are invalid and dropped: ",
		paste(deparse(dotargs[named]), collapse=", "))
    dotargs <- dotargs[!named]
    is.glm <- vapply(dotargs,function(x) inherits(x,"glm"), NA)
    dotargs <- dotargs[is.glm]

    ## do not copy this: anova.glmlist is not an exported object.
    ## use anova(structure(list(object, dotargs), class = "glmlist"))
    if (length(dotargs))
	return(anova.glmlist(c(list(object), dotargs),
			     dispersion = dispersion, test = test))

    ## score tests require a bit of extra computing
    doscore <- !is.null(test) && test=="Rao"
    ## extract variables from model

    varlist <- attr(object$terms, "variables")
    ## must avoid partial matching here.
    x <-
	if (n <- match("x", names(object), 0L))
	    object[[n]]
	else model.matrix(object)
    varseq <- attr(x, "assign")
    nvars <- max(0, varseq)
    resdev <- resdf <- NULL

    if (doscore){
      score <- numeric(nvars)
      # fit a null model
      method <- object$method
      y <- object$y
      fit <- eval(call(if(is.function(method)) "method" else method,
                       x=x[, varseq == 0, drop = FALSE],
                       y=y,
                       weights=object$prior.weights,
                       start  =object$start,
                       offset =object$offset,
                       family =object$family,
                       control=object$control))
      r <- fit$residuals
      w <- fit$weights
    }

    ## if there is more than one explanatory variable then
    ## recall glm.fit to fit variables sequentially

    ## for score tests, we need to do so in any case
    if(nvars > 1 || doscore) {
	method <- object$method
        ## allow for 'y = FALSE' in the call (PR#13098)
        y <- object$y
        if(is.null(y)) { ## code from residuals.glm
            mu.eta <- object$family$mu.eta
            eta <- object$linear.predictors
            y <- object$fitted.values + object$residuals * mu.eta(eta)
        }
	for(i in seq_len(nvars-1L)) {
	    ## explanatory variables up to i are kept in the model
	    ## use method from glm to find residual deviance
	    ## and df for each sequential fit
	    fit <- eval(call(if(is.function(method)) "method" else method,
                             x=x[, varseq <= i, drop = FALSE],
                             y=y,
                             weights=object$prior.weights,
                             start  =object$start,
                             offset =object$offset,
                             family =object$family,
                             control=object$control))
            if (doscore) {
              zz <- eval(call(if(is.function(method)) "method" else method,
                             x=x[, varseq <= i, drop = FALSE],
                             y=r,
                             weights=w))
              score[i] <-  zz$null.deviance - zz$deviance
              r <- fit$residuals
              w <- fit$weights
            }
	    resdev <- c(resdev, fit$deviance)
	    resdf <- c(resdf, fit$df.residual)
	}
        if (doscore) {
          zz <- eval(call(if(is.function(method)) "method" else method,
                          x=x,
                          y=r,
                          weights=w))
          score[nvars] <- zz$null.deviance - zz$deviance
        }
    }

    ## add values from null and full model

    resdf <- c(object$df.null, resdf, object$df.residual)
    resdev <- c(object$null.deviance, resdev, object$deviance)

    ## construct table and title

    table <- data.frame(c(NA, -diff(resdf)),
			c(NA, pmax(0, -diff(resdev))), resdf, resdev)
    tl <- attr(object$terms, "term.labels")
    if (length(tl) == 0L) table <- table[1,,drop=FALSE] # kludge for null model
    dimnames(table) <- list(c("NULL", tl),
			    c("Df", "Deviance", "Resid. Df", "Resid. Dev"))
    if (doscore)
      table <- cbind(table, Rao=c(NA,score))
    title <- paste0("Analysis of Deviance Table", "\n\nModel: ",
                    object$family$family, ", link: ", object$family$link,
                    "\n\nResponse: ", as.character(varlist[-1L])[1L],
                    "\n\nTerms added sequentially (first to last)\n\n")

    ## calculate test statistics if needed

    df.dispersion <- Inf
    if(is.null(dispersion)) {
	dispersion <- summary(object, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else object$df.residual
    }
    if(!is.null(test)) {
        if(test == "F" && df.dispersion == Inf) {
            fam <- object$family$family
            if(fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a '%s' family is inappropriate",
                                 fam),
                        domain = NA)
            else
                warning("using F test with a fixed dispersion is inappropriate")
        }
	table <- stat.anova(table=table, test=test, scale=dispersion,
			    df.scale=df.dispersion, n=NROW(x))
    }
    structure(table, heading = title, class = c("anova", "data.frame"))
}


anova.glmlist <- function(object, ..., dispersion=NULL, test=NULL)
{

    doscore <- !is.null(test) && test=="Rao"

    ## find responses for all models and remove
    ## any models with a different response

    responses <- as.character(lapply(object, function(x) {
	deparse(formula(x)[[2L]])} ))
    sameresp <- responses==responses[1L]
    if(!all(sameresp)) {
	object <- object[sameresp]
        warning(gettextf("models with response %s removed because response differs from model 1",
                         sQuote(deparse(responses[!sameresp]))),
                domain = NA)
    }

    ns <- sapply(object, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
	stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models

    nmodels <- length(object)
    if(nmodels==1)
	return(anova.glm(object[[1L]], dispersion=dispersion, test=test))

    ## extract statistics

    resdf  <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))

    if (doscore){
      score <- numeric(nmodels)
      score[1] <- NA
      df <- -diff(resdf)

      for (i in seq_len(nmodels-1)) {
        m1 <- if (df[i]>0) object[[i]] else object[[i+1]]
        m2 <- if (df[i]>0) object[[i+1]] else object[[i]]
        r <- m1$residuals
        w <- m1$weights
        method <- m2$method
        zz <- eval(call(if(is.function(method)) "method" else method,
                        x=model.matrix(m2),
                        y=r,
                        weights=w))
        score[i+1] <-  zz$null.deviance - zz$deviance
        if (df < 0) score[i+1] <- - score[i+1]
      }
    }

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
			c(NA, -diff(resdev)) )
    variables <- lapply(object, function(x)
			paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1L:nmodels, c("Resid. Df", "Resid. Dev", "Df",
					 "Deviance"))
    if (doscore)
      table <- cbind(table, Rao=score)

    title <- "Analysis of Deviance Table\n"
    topnote <- paste0("Model ", format(1L:nmodels), ": ", variables,
                      collapse = "\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- object[[order(resdf)[1L]]]
	dispersion <- summary(bigmodel, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else min(resdf)
        if(test == "F" && df.dispersion == Inf) {
            fam <- bigmodel$family$family
            if(fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a '%s' family is inappropriate",
                                 fam),
                        domain = NA, call. = FALSE)
            else
                warning("using F test with a fixed dispersion is inappropriate")
        }
	table <- stat.anova(table = table, test = test,
			    scale = dispersion, df.scale = df.dispersion,
			    n = length(bigmodel$residuals))
    }
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame"))
}


summary.glm <- function(object, dispersion = NULL,
			correlation = FALSE, symbolic.cor = FALSE, ...)
{
    est.disp <- FALSE
    df.r <- object$df.residual
    if(is.null(dispersion))	# calculate dispersion if needed
	dispersion <-
	    if(object$family$family %in% c("poisson", "binomial"))  1
	    else if(df.r > 0) {
                est.disp <- TRUE
		if(any(object$weights==0))
		    warning("observations with zero weight not used for calculating dispersion")
		sum((object$weights*object$residuals^2)[object$weights > 0])/ df.r
	    } else {
                est.disp <- TRUE
                NaN
            }

    ## calculate scaled and unscaled covariance matrix

    aliased <- is.na(coef(object))  # used in print method
    p <- object$rank
    if (p > 0) {
        p1 <- 1L:p
	Qr <- qr.lm(object)
        ## WATCHIT! doesn't this rely on pivoting not permuting 1L:p? -- that's quaranteed
        coef.p <- object$coefficients[Qr$pivot[p1]]
        covmat.unscaled <- chol2inv(Qr$qr[p1,p1,drop=FALSE])
        dimnames(covmat.unscaled) <- list(names(coef.p),names(coef.p))
        covmat <- dispersion*covmat.unscaled
        var.cf <- diag(covmat)

        ## calculate coef table

        s.err <- sqrt(var.cf)
        tvalue <- coef.p/s.err

        dn <- c("Estimate", "Std. Error")
        if(!est.disp) { # known dispersion
            pvalue <- 2*pnorm(-abs(tvalue))
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "z value","Pr(>|z|)"))
        } else if(df.r > 0) {
            pvalue <- 2*pt(-abs(tvalue), df.r)
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "t value","Pr(>|t|)"))
        } else { # df.r == 0
            coef.table <- cbind(coef.p, NaN, NaN, NaN)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "t value","Pr(>|t|)"))
        }
        df.f <- NCOL(Qr$qr)
    } else {
        coef.table <- matrix(, 0L, 4L)
        dimnames(coef.table) <-
            list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        covmat.unscaled <- covmat <- matrix(, 0L, 0L)
        df.f <- length(aliased)
    }
    ## return answer

    ## these need not all exist, e.g. na.action.
    keep <- match(c("call","terms","family","deviance", "aic",
		      "contrasts", "df.residual","null.deviance","df.null",
                      "iter", "na.action"), names(object), 0L)
    ans <- c(object[keep],
	     list(deviance.resid = residuals(object, type = "deviance"),
		  coefficients = coef.table,
                  aliased = aliased,
		  dispersion = dispersion,
		  df = c(object$rank, df.r, df.f),
		  cov.unscaled = covmat.unscaled,
		  cov.scaled = covmat))

    if(correlation && p > 0) {
	dd <- sqrt(diag(covmat.unscaled))
	ans$correlation <-
	    covmat.unscaled/outer(dd,dd)
	ans$symbolic.cor <- symbolic.cor
    }
    class(ans) <- "summary.glm"
    return(ans)
}

print.summary.glm <-
    function (x, digits = max(3L, getOption("digits") - 3L),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nCall:\n",
	paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    cat("Deviance Residuals: \n")
    if(x$df.residual > 5) {
	x$deviance.resid <- setNames(quantile(x$deviance.resid, na.rm = TRUE),
				     c("Min", "1Q", "Median", "3Q", "Max"))
    }
    xx <- zapsmall(x$deviance.resid, digits + 1L)
    print.default(xx, digits = digits, na.print = "", print.gap = 2L)

    if(length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        ## df component added in 1.8.0
        ## partial matching problem here.
        df <- if ("df" %in% names(x)) x[["df"]] else NULL
        if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4L,
                            dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                     na.print = "NA", ...)
    }
    ##
    cat("\n(Dispersion parameter for ", x$family$family,
	" family taken to be ", format(x$dispersion), ")\n\n",
	apply(cbind(paste(format(c("Null","Residual"), justify="right"),
                          "deviance:"),
		    format(unlist(x[c("null.deviance","deviance")]),
			   digits = max(5L, digits + 1L)), " on",
		    format(unlist(x[c("df.null","df.residual")])),
		    " degrees of freedom\n"),
	      1L, paste, collapse = " "), sep = "")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
    cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)),"\n\n",
	"Number of Fisher Scoring iterations: ", x$iter,
	"\n", sep = "")

    correl <- x$correlation
    if(!is.null(correl)) {
# looks most sensible not to give NAs for undefined coefficients
#         if(!is.null(aliased) && any(aliased)) {
#             nc <- length(aliased)
#             correl <- matrix(NA, nc, nc, dimnames = list(cn, cn))
#             correl[!aliased, !aliased] <- x$correl
#         }
	p <- NCOL(correl)
	if(p > 1) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
		print(symnum(correl, abbr.colnames = NULL))
	    } else {
		correl <- format(round(correl, 2L), nsmall = 2L,
                                 digits = digits)
		correl[!lower.tri(correl)] <- ""
		print(correl[-1, -p, drop=FALSE], quote = FALSE)
	    }
	}
    }
    cat("\n")
    invisible(x)
}


## GLM Methods for Generic Functions :

## needed to avoid deviance.lm
deviance.glm <- function(object, ...) object$deviance
effects.glm <- function(object, ...) object$effects
family.glm <- function(object, ...) object$family

residuals.glm <-
    function(object,
	     type = c("deviance", "pearson", "working", "response", "partial"),
	     ...)
{
    type <- match.arg(type)
    y <- object$y
    r <- object$residuals
    mu <- object$fitted.values
    wts <- object$prior.weights
    switch(type,
           deviance=,pearson=,response=
           if(is.null(y)) {
               mu.eta <- object$family$mu.eta
               eta <- object$linear.predictors
               y <-  mu + r * mu.eta(eta)
           })
    res <- switch(type,
		  deviance = if(object$df.residual > 0) {
		      d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, wts), 0))
		      ifelse(y > mu, d.res, -d.res)
		  } else rep.int(0, length(mu)),
		  pearson = (y-mu)*sqrt(wts)/sqrt(object$family$variance(mu)),
		  working = r,
		  response = y - mu,
		  partial = r
		  )
    if(!is.null(object$na.action))
        res <- naresid(object$na.action, res)
    if (type == "partial") ## need to avoid doing naresid() twice.
        res <- res+predict(object, type="terms")
    res
}

## For influence.glm() ... --> ./lm.influence.R

## KH on 1998/06/22: update.default() is now used ...

model.frame.glm <- function (formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0L)]
    if (length(nargs) || is.null(formula$model)) {
	fcall <- formula$call
	fcall$method <- "model.frame"
        ## need stats:: for non-standard evaluation
	fcall[[1L]] <- quote(stats::glm)
        fcall[names(nargs)] <- nargs
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
	eval(fcall, env)
    }
    else formula$model
}

weights.glm <- function(object, type = c("prior", "working"), ...)
{
    type <- match.arg(type)
    res <- if(type == "prior") object$prior.weights else object$weights
    if(is.null(object$na.action)) res
    else naresid(object$na.action, res)
}

formula.glm <- function(x, ...)
{
    form <- x$formula
    if( !is.null(form) ) {
        form <- formula(x$terms) # has . expanded
        environment(form) <- environment(x$formula)
        form
    } else formula(x$terms)
}
