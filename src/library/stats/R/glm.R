### This function fits a generalized linear model via
### iteratively reweighted least squares for any family.
### Written by Simon Davies, Dec 1995
### glm.fit modified by Thomas Lumley, Apr 1997, and then others..

glm <- function(formula, family = gaussian, data, weights,
		subset, na.action, start = NULL,
		etastart, mustart, offset,
		control = glm.control(...), model = TRUE,
                method = "glm.fit", x = FALSE, y = TRUE,
                contrasts = NULL, ...)
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
#    mt <- terms(formula, data = data)
    if(missing(data)) data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
#     mf$family <- mf$start <- mf$control <- mf$maxit <- NULL
#     mf$model <- mf$method <- mf$x <- mf$y <- mf$contrasts <- NULL
#     mf$... <- NULL
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "etastart", "mustart", "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    switch(method,
	   "model.frame" = return(mf),
	   "glm.fit" = 1,
	   "glm.fit.null" = 1,
	   ## else
	   stop("invalid 'method': ", method))
    mt <- attr(mf, "terms") # allow model.frame to update it

    Y <- model.response(mf, "numeric")
    ## avoid problems with 1D arrays, but keep names
    if(length(dim(Y)) == 1) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if(!is.null(nm)) names(Y) <- nm
    }
    ## null model support
    X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts) else matrix(,NROW(Y),0)
    weights <- model.weights(mf)
    offset <- model.offset(mf)
    ## check weights and offset
    if( !is.null(weights) && any(weights < 0) )
	stop("negative weights not allowed")
    if(!is.null(offset) && length(offset) != NROW(Y))
	stop(gettextf("number of offsets is %d should equal %d (number of observations)", length(offset), NROW(Y)), domain = NA)
    ## these allow starting values to be expressed in terms of other vars.
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")

    ## fit model via iterative reweighted least squares
    fit <- glm.fit(x = X, y = Y, weights = weights, start = start,
                   etastart = etastart, mustart = mustart,
                   offset = offset, family = family, control = control,
                   intercept = attr(mt, "intercept") > 0)

    ## empty models don't have an intercept!
    if(any(offset) && attr(mt, "intercept") > 0) {
	fit$null.deviance <-
	    glm.fit(x = X[,"(Intercept)",drop=FALSE], y = Y, weights = weights,
                    offset = offset, family = family,
                    control = control, intercept = TRUE)$deviance
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
    class(fit) <- c("glm", "lm")
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
	      family = gaussian(), control = glm.control(), intercept = TRUE)
{
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2]]
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
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv) )
	stop("'family' argument seems not to be a valid family object")
    valideta <- family$valideta
    if (is.null(valideta))
	valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu))
	validmu <- function(mu) TRUE
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
            stop("invalid linear predictor values in empty model")
        mu <- linkinv(eta)
        ## calculate initial deviance and coefficient
        if (!validmu(mu))
            stop("invalid fitted means in empty model")
        dev <- sum(dev.resids(y, mu, weights))
        w <- ((weights * mu.eta(eta)^2)/variance(mu))^0.5
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric(0)
        iter <- 0
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
                    offset + as.vector(if (NCOL(x) == 1) x * start else x %*% start)
                }
            else family$linkfun(mustart)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta)))
            stop("cannot find valid starting values: please specify some")
        ## calculate initial deviance and coefficient
        devold <- sum(dev.resids(y, mu, weights))
        boundary <- conv <- FALSE

        ##------------- THE Iteratively Reweighting L.S. iteration -----------
        for (iter in 1:control$maxit) {
            good <- weights > 0
            varmu <- variance(mu)[good]
            if (any(is.na(varmu)))
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
                warning("no observations informative at iteration ", iter)
                break
            }
            z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
            w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
            ngoodobs <- as.integer(nobs - sum(!good))
            ## call Fortran code
            fit <- .Fortran("dqrls",
                            qr = x[good, ] * w, n = ngoodobs,
                            p = nvars, y = w * z, ny = as.integer(1),
                            tol = min(1e-7, control$epsilon/1000),
                            coefficients = double(nvars),
                            residuals = double(ngoodobs),
                            effects = double(ngoodobs),
                            rank = integer(1),
                            pivot = 1:nvars, qraux = double(nvars),
                            work = double(2 * nvars),
                            PACKAGE = "base")
            if (any(!is.finite(fit$coefficients))) {
                conv <- FALSE
                warning("non-finite coefficients at iteration ", iter)
                break
            }
            ## stop if not enough parameters
            if (nobs < fit$rank)
                stop(gettextf("X matrix has rank %d, but only %d observations",
                              fit$rank, nobs), domain = NA)
            ## calculate updated values of eta and mu with the new coef:
            start[fit$pivot] <- fit$coefficients
            eta <- drop(x %*% start)
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace)
                cat("Deviance =", dev, "Iterations -", iter, "\n")
            ## check for divergence
            boundary <- FALSE
            if (!is.finite(dev)) {
                if(is.null(coefold))
                    stop("no valid set of coefficients has been found: please supply starting values", call. = FALSE)
                warning("step size truncated due to divergence", call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                    if (ii > control$maxit)
                        stop("inner loop 1; cannot correct step size")
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                    dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace)
                    cat("Step halved: new deviance =", dev, "\n")
            }
            ## check for fitted values outside domain.
            if (!(valideta(eta) && validmu(mu))) {
                warning("step size truncated: out of bounds", call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                    if (ii > control$maxit)
                        stop("inner loop 2; cannot correct step size")
                    ii <- ii + 1
                    start <- (start + coefold)/2
                    eta <- drop(x %*% start)
                    mu <- linkinv(eta <- eta + offset)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace)
                    cat("Step halved: new deviance =", dev, "\n")
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

        if (!conv) warning("algorithm did not converge")
        if (boundary) warning("algorithm stopped at boundary value")
        eps <- 10*.Machine$double.eps
        if (family$family == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps))
                warning("fitted probabilities numerically 0 or 1 occurred")
        }
        if (family$family == "poisson") {
            if (any(mu < eps))
                warning("fitted rates numerically 0 occurred")
        }
        ## If X matrix was not full rank then columns were pivoted,
        ## hence we need to re-label the names ...
        ## Original code changed as suggested by BDR---give NA rather
        ## than 0 for non-estimable parameters
        if (fit$rank < nvars) coef[fit$pivot][seq(fit$rank+1, nvars)] <- NA
        xxnames <- xnames[fit$pivot]
        residuals <- rep.int(NA, nobs)
        residuals[good] <- z - (eta - offset)[good] # z does not have offset in.
        fit$qr <- as.matrix(fit$qr)
        nr <- min(sum(good), nvars)
        if (nr < nvars) {
            Rmat <- diag(nvars)
            Rmat[1:nr, 1:nvars] <- fit$qr[1:nr, 1:nvars]
        }
        else Rmat <- fit$qr[1:nvars, 1:nvars]
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
            c(xxnames[seq(len=fit$rank)], rep.int("", sum(good) - fit$rank))
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
	 qr = if(!EMPTY) structure(fit[c("qr", "rank", "qraux", "pivot", "tol")], class="qr"),
         family = family,
	 linear.predictors = eta, deviance = dev, aic = aic.model,
	 null.deviance = nulldev, iter = iter, weights = wt,
	 prior.weights = weights, df.residual = resdf, df.null = nulldf,
	 y = y, converged = conv, boundary = boundary)
}


print.glm <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    if(length(coef(x))) {
        cat("Coefficients")
        if(is.character(co <- x$contrasts))
            cat("  [contrasts: ",
                apply(cbind(names(co),co), 1, paste, collapse="="), "]")
        cat(":\n")
        print.default(format(x$coefficients, digits=digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    cat("Null Deviance:	   ",	format(signif(x$null.deviance, digits)),
	"\nResidual Deviance:", format(signif(x$deviance, digits)),
	"\tAIC:", format(signif(x$aic, digits)), "\n")
    invisible(x)
}


anova.glm <- function(object, ..., dispersion=NULL, test=NULL)
{
    ## check for multiple objects
    dotargs <- list(...)
    named <- if (is.null(names(dotargs)))
	rep(FALSE, length(dotargs)) else (names(dotargs) != "")
    if(any(named))
	warning("the following arguments to 'anova.glm' are invalid and dropped: ",
		paste(deparse(dotargs[named]), collapse=", "))
    dotargs <- dotargs[!named]
    is.glm <- unlist(lapply(dotargs,function(x) inherits(x,"glm")))
    dotargs <- dotargs[is.glm]
    if (length(dotargs) > 0)
	return(anova.glmlist(c(list(object), dotargs),
			     dispersion = dispersion, test=test))

    ## extract variables from model

    varlist <- attr(object$terms, "variables")
    ## must avoid partial matching here.
    x <-
	if (n <- match("x", names(object), 0))
	    object[[n]]
	else model.matrix(object)
    varseq <- attr(x, "assign")
    nvars <- max(0, varseq)
    resdev <- resdf <- NULL

    ## if there is more than one explanatory variable then
    ## recall glm.fit to fit variables sequentially

    if(nvars > 1) {
	method <- object$method
	if(!is.function(method))
	    method <- get(method, mode = "function", envir=parent.frame())
	for(i in 1:(nvars-1)) {
	    ## explanatory variables up to i are kept in the model
	    ## use method from glm to find residual deviance
	    ## and df for each sequential fit
	    fit <- method(x=x[, varseq <= i, drop = FALSE],
			  y=object$y,
			  weights=object$prior.weights,
			  start	 =object$start,
			  offset =object$offset,
			  family =object$family,
			  control=object$control)
	    resdev <- c(resdev, fit$deviance)
	    resdf <- c(resdf, fit$df.residual)
	}
    }

    ## add values from null and full model

    resdf <- c(object$df.null, resdf, object$df.residual)
    resdev <- c(object$null.deviance, resdev, object$deviance)

    ## construct table and title

    table <- data.frame(c(NA, -diff(resdf)),
			c(NA, pmax(0, -diff(resdev))), resdf, resdev)
    tl <- attr(object$terms, "term.labels")
    if (length(tl) == 0) table <- table[1,,drop=FALSE] # kludge for null model
    dimnames(table) <- list(c("NULL", tl),
			    c("Df", "Deviance", "Resid. Df", "Resid. Dev"))
    title <- paste("Analysis of Deviance Table", "\n\nModel: ",
		   object$family$family, ", link: ", object$family$link,
		   "\n\nResponse: ", as.character(varlist[-1])[1],
		   "\n\nTerms added sequentially (first to last)\n\n", sep="")

    ## calculate test statistics if needed

    df.dispersion <- Inf
    if(is.null(dispersion)) {
	dispersion <- summary(object, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else object$df.residual
    }
    if(!is.null(test))
	table <- stat.anova(table=table, test=test, scale=dispersion,
			    df.scale=df.dispersion, n=NROW(x))
    structure(table, heading = title, class= c("anova", "data.frame"))
}


anova.glmlist <- function(object, ..., dispersion=NULL, test=NULL)
{

    ## find responses for all models and remove
    ## any models with a different response

    responses <- as.character(lapply(object, function(x) {
	deparse(formula(x)[[2]])} ))
    sameresp <- responses==responses[1]
    if(!all(sameresp)) {
	object <- object[sameresp]
	warning("models with response ", deparse(responses[!sameresp]),
		" removed because response differs from model 1")
    }

    ns <- sapply(object, function(x) length(x$residuals))
    if(any(ns != ns[1]))
	stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models

    nmodels <- length(object)
    if(nmodels==1)
	return(anova.glm(object[[1]], dispersion=dispersion, test=test))

    ## extract statistics

    resdf  <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
			c(NA, -diff(resdev)) )
    variables <- lapply(object, function(x)
			paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1:nmodels, c("Resid. Df", "Resid. Dev", "Df",
					 "Deviance"))
    title <- "Analysis of Deviance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     variables, sep="", collapse="\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- object[[order(resdf)[1]]]
	dispersion <- summary(bigmodel, dispersion=dispersion)$dispersion
	df.dispersion <- if (dispersion == 1) Inf else min(resdf)
	table <- stat.anova(table = table, test = test,
			    scale = dispersion, df.scale = df.dispersion,
			    n = length(bigmodel$residuals))
    }
    structure(table, heading = c(title, topnote),
	      class = c("anova", "data.frame"))
}


## utility for anova.FOO(), FOO in "lmlist", "glm", "glmlist":
stat.anova <- function(table, test=c("Chisq", "F", "Cp"), scale, df.scale, n)
{
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if(is.na(dev.col)) dev.col <- match("Sum of Sq", colnames(table))
    switch(test,
	   "Chisq" = {
	       cbind(table,"P(>|Chi|)"= pchisq(abs(table[, dev.col]/scale),
			     abs(table[, "Df"]), lower.tail=FALSE))
	   },
	   "F" = {
	       Fvalue <- abs((table[, dev.col]/table[, "Df"])/scale)
	       Fvalue[table[, "Df"] %in% 0] <- NA
	       cbind(table, F = Fvalue,
		     "Pr(>F)" = pf(Fvalue, abs(table[, "Df"]),
		     abs(df.scale), lower.tail=FALSE))
	   },
	   "Cp" = {
	       cbind(table, Cp = table[,"Resid. Dev"] +
		     2*scale*(n - table[,"Resid. Df"]))
	   })
}

summary.glm <- function(object, dispersion = NULL,
			correlation = FALSE, symbolic.cor = FALSE, ...)
{
    est.disp <- FALSE
    df.r <- object$df.residual
    if(is.null(dispersion))	# calculate dispersion if needed
	dispersion <-
	    if(any(object$family$family == c("poisson", "binomial")))  1
	    else if(df.r > 0) {
		est.disp <- TRUE
		if(any(object$weights==0))
		    warning("observations with zero weight not used for calculating dispersion")
		sum(object$weights*object$residuals^2)/ df.r
	    } else Inf

    ## calculate scaled and unscaled covariance matrix

    p <- object$rank
    if (p > 0) {
        p1 <- 1:p
        Qr <- object$qr
        aliased <- is.na(coef(object))  # used in print method
        ## WATCHIT! doesn't this rely on pivoting not permuting 1:p? -- that's quaranteed
        coef.p <- object$coefficients[Qr$pivot[p1]]
        covmat.unscaled <- chol2inv(Qr$qr[p1,p1,drop=FALSE])
        dimnames(covmat.unscaled) <- list(names(coef.p),names(coef.p))
        covmat <- dispersion*covmat.unscaled
        var.cf <- diag(covmat)

        ## calculate coef table

        s.err <- sqrt(var.cf)
        tvalue <- coef.p/s.err

        dn <- c("Estimate", "Std. Error")
        if(!est.disp) {
            pvalue <- 2*pnorm(-abs(tvalue))
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "z value","Pr(>|z|)"))
        } else if(df.r > 0) {
            pvalue <- 2*pt(-abs(tvalue), df.r)
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p),
                                         c(dn, "t value","Pr(>|t|)"))
        } else { ## df.r == 0
            coef.table <- cbind(coef.p, Inf)
            dimnames(coef.table) <- list(names(coef.p), dn)
        }
        df.f <- NCOL(Qr$qr)
    } else {
        coef.table <- matrix(, 0, 4)
        dimnames(coef.table) <-
            list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        covmat.unscaled <- covmat <- matrix(, 0, 0)
        aliased <- is.na(coef(object))
        df.f <- length(aliased)
    }
    ## return answer

    ans <- c(object[c("call","terms","family","deviance", "aic",
		      "contrasts",
		      "df.residual","null.deviance","df.null","iter")],
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
    function (x, digits = max(3, getOption("digits") - 3),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
    cat("Deviance Residuals: \n")
    if(x$df.residual > 5) {
	x$deviance.resid <- quantile(x$deviance.resid,na.rm=TRUE)
	names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    print.default(x$deviance.resid, digits=digits, na = "", print.gap = 2)

    if(length(x$aliased) == 0) {
        cat("\nNo Coefficients\n")
    } else {
        ## df component added in 1.8.0
        if (!is.null(df<- x$df) && (nsingular <- df[3] - df[1]))
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4,
                            dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits=digits, signif.stars=signif.stars,
                     na.print="NA", ...)
    }
    ##
    cat("\n(Dispersion parameter for ", x$family$family,
	" family taken to be ", format(x$dispersion), ")\n\n",
	apply(cbind(paste(format.char(c("Null","Residual"),width=8,flag=""),
			  "deviance:"),
		    format(unlist(x[c("null.deviance","deviance")]),
			   digits= max(5, digits+1)), " on",
		    format(unlist(x[c("df.null","df.residual")])),
		    " degrees of freedom\n"),
	      1, paste, collapse=" "),
	"AIC: ", format(x$aic, digits= max(4, digits+1)),"\n\n",
	"Number of Fisher Scoring iterations: ", x$iter,
	"\n", sep="")

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
		print(symnum(correl, abbr.col = NULL))
	    } else {
		correl <- format(round(correl, 2), nsmall = 2, digits = digits)
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
    mu	<- object$fitted.values
    wts <- object$prior.weights
    res <- switch(type,
		  deviance = if(object$df.res > 0) {
		      d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, wts), 0))
		      ifelse(y > mu, d.res, -d.res)
		  } else rep.int(0, length(mu)),
		  pearson = (y-mu)*sqrt(wts)/sqrt(object$family$variance(mu)),
		  working = r,
		  response = y - mu,
		  partial = r
		  )
    if(!is.null(object$na.action))
      res<- naresid(object$na.action, res)
    if (type=="partial") ## need to avoid doing naresid() twice.
      res<-res+predict(object, type="terms")
    res
}

## KH on 1998/06/22: update.default() is now used ...

model.frame.glm <- function (formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if (length(nargs) || is.null(formula$model)) {
	fcall <- formula$call
	fcall$method <- "model.frame"
	fcall[[1]] <- as.name("glm")
        fcall[names(nargs)] <- nargs
#	env <- environment(fcall$formula)  # always NULL
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
