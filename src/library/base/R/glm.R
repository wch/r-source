### This function fits a generalized linear model via
### iteratively reweighted least squares for any family.
### Written by Simon Davies, Dec 1995
### glm.fit modified by Thomas Lumley, Apr 1997, and then others..

glm <- function(formula, family=gaussian, data=list(), weights=NULL,
		subset=NULL, na.action=na.fail, start=NULL, offset=NULL,
		control=glm.control(...), model=TRUE, method="glm.fit",
                x=FALSE, y=TRUE, contrasts = NULL, ...)
{
    call <- match.call()

    ## family
    if(is.character(family)) family <- get(family)
    if(is.function(family)) family <- family()
    if(is.null(family$family)) {
	print(family)
	stop("`family' not recognized")
    }

    ## extract x, y, etc from the model formula and frame
    mt <- terms(formula, data=data)
    if(missing(data)) data <- sys.frame(sys.parent())
    mf <- match.call(expand.dots = FALSE)
    mf$family <- mf$start <- mf$control <- mf$maxit <- NULL
    mf$model <- mf$method <- mf$x <- mf$y <- mf$contrasts <- NULL
    mf$... <- NULL
    ##	      mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, sys.frame(sys.parent()))
    switch(method,
	   "model.frame" = return(mf),
	   "glm.fit"= 1,
	   "glm.fit.null"= 1,
	   ## else
	   stop(paste("invalid `method':", method)))
    xvars <- as.character(attr(mt, "variables"))[-1]
    if((yvar <- attr(mt, "response")) > 0) xvars <- xvars[-yvar]
    xlev <- if(length(xvars) > 0) {
	xlev <- lapply(mf[xvars], levels)
	xlev[!sapply(xlev, is.null)]
    } # else NULL

    ## null model support
    X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts)# else NULL
    Y <- model.response(mf, "numeric")
    weights <- model.weights(mf)
    offset <- model.offset(mf)
    ## check weights and offset
    if( !is.null(weights) && any(weights<0) )
	stop("Negative wts not allowed")
    if(!is.null(offset) && length(offset) != NROW(Y))
	stop(paste("Number of offsets is", length(offset),
		   ", should equal", NROW(Y), "(number of observations)"))

    ## fit model via iterative reweighted least squares
    fit <-
        (if (is.empty.model(mt))
         glm.fit.null else glm.fit)(x=X, y=Y, weights=weights, start=start,
                                    offset=offset,family=family,control=control,
                                    intercept=attr(mt, "intercept") > 0)

    if(any(offset) && attr(mt, "intercept") > 0) {
	fit$null.deviance <-
	    if(is.empty.model(mt)) fit$deviance
	    else glm.fit(x=X[,"(Intercept)",drop=FALSE], y=Y, weights=weights,
			 start=start, offset=offset, family=family,
			 control=control, intercept=TRUE)$deviance
    }
    if(model) fit$model <- mf
    if(x) fit$x <- X
    if(!y) fit$y <- NULL
    fit <- c(fit, list(call=call, formula=formula,
		       terms=mt, data=data,
		       offset=offset, control=control, method=method,
		       contrasts = attr(X, "contrasts"), xlevels = xlev))
    class(fit) <- c(if(is.empty.model(mt)) "glm.null", "glm", "lm")
    fit
}


glm.control <- function(epsilon = 0.0001, maxit = 10, trace = FALSE)
{
    if(!is.numeric(epsilon) || epsilon <= 0)
	stop("value of epsilon must be > 0")
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
    ynames <- names(y)
    conv <- FALSE
    nobs <- NROW(y)
    nvars <- NCOL(x)
    if (nvars == 0) {
        ## oops, you'd want glm.fit.null, then
        cc <- match.call()
        cc[[1]] <- as.name("glm.fit.null")
        return(eval(cc, sys.frame(sys.parent())))
    }
    ## define weights and offset if needed
    if (is.null(weights))
	weights <- rep(1, nobs)
    if (is.null(offset))
	offset <- rep(0, nobs)
    ## get family functions:
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv) )
	stop("illegal `family' argument")
    valideta <- family$valideta
    if (is.null(valideta))
	valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu))
	validmu <- function(mu) TRUE
    if(is.null(mustart))
	## next line calculates mustart and may change y and weights
	eval(family$initialize, sys.frame(sys.nframe()))
    if (NCOL(y) > 1)
	stop("y must be univariate unless binomial")
    eta <-
	if(!is.null(etastart) && valideta(etastart))
	    etastart
	else if(!is.null(start))
	    if (length(start) != nvars)
		stop(paste("Length of start should equal", nvars,
			   "and correspond to initial coefs for",
			   deparse(xnames)))
	    else as.vector(if (NCOL(x) == 1) x * start else x %*% start)
	else family$linkfun(mustart)
    mu <- linkinv(eta)
    if (!(validmu(mu) && valideta(eta)))
	stop("Can't find valid starting values: please specify some")
    ## calculate initial deviance and coefficient
    devold <- sum(dev.resids(y, mu, weights))
    coefold <- start
    boundary <- FALSE

    ##------------- THE Iteratively Reweighting L.S. iteration -----------
    for (iter in 1:control$maxit) {
	mu.eta.val <- mu.eta(eta)
	if (any(ina <- is.na(mu.eta.val)))
	    mu.eta.val[ina] <- mu.eta(mu)[ina]
	if (any(is.na(mu.eta.val)))
	    stop("NAs in d(mu)/d(eta)")

	## calculate z and w using only values where mu.eta != 0
	good <- mu.eta.val != 0
	if (all(!good)) {
	    conv <- FALSE
	    warning(paste("No observations informative at iteration",
			  iter))
	    break
	}
	z <- (eta-offset)[good] + (y - mu)[good]/mu.eta.val[good]
	w <- sqrt((weights * mu.eta.val^2)[good]/variance(mu)[good])
	ngoodobs <- as.integer(nobs - sum(!good))
	ncols <- as.integer(1)
	## call linpack code
	fit <- .Fortran("dqrls",
			qr = x[good, ] * w, n = as.integer(ngoodobs),
			p = nvars, y = w * z, ny = ncols,
			tol = min(1e-7, control$epsilon/1000),
			coefficients = numeric(nvars),
			residuals = numeric(ngoodobs),
			effects = numeric(ngoodobs),
			rank = integer(1),
			pivot = 1:nvars, qraux = double(nvars),
			work = double(2 * nvars),
                        PACKAGE = "base")
	## stop if not enough parameters
	if (nobs < fit$rank)
	    stop(paste("X matrix has rank", fit$rank, "but only",
		       nobs, "observations"))
	## calculate updated values of eta and mu with the new coef:
	start <- coef <- fit$coefficients
	start[fit$pivot] <- coef
	eta[good] <-
	    if (nvars == 1) x[good] * start else as.vector(x[good, ] %*% start)
	mu <- linkinv(eta <- eta + offset)
	if (family$family == "binomial") {
	    if (any(mu == 1) || any(mu == 0))
		warning("fitted probabilities of 0 or 1 occurred")
	    mu0 <- 0.5 * control$epsilon/length(mu)
	    mu[mu == 1] <- 1 - mu0
	    mu[mu == 0] <- mu0
	}
	else if (family$family == "poisson") {
	    if (any(mu == 0))
		warning("fitted rates of 0 occured")
	    mu[mu == 0] <- 0.5 * control$epsilon/length(mu)^2
	}
	dev <- sum(dev.resids(y, mu, weights))
	if (control$trace)
	    cat("Deviance =", dev, "Iterations -", iter, "\n")
	## check for divergence
	boundary <- FALSE
	if (any(is.na(dev)) || any(is.na(coef))) {
	    warning("Step size truncated due to divergence")
	    ii <- 1
	    while ((any(is.na(dev)) || any(is.na(start)))) {
		if (ii > control$maxit)
		    stop("inner loop 1; can't correct step size")
		ii <- ii+1
		start <- (start + coefold)/2
		eta[good] <-
		    if (nvars == 1) x[good] * start else as.vector(x[good, ] %*% start)
		mu <- linkinv(eta <- eta + offset)
		dev <- sum(dev.resids(y, mu, weights))
	    }
	    boundary <- TRUE
	    coef <- start
	    if (control$trace)
		cat("New Deviance =", dev, "\n")
	}
	## check for fitted values outside domain.
	if (!(valideta(eta) && validmu(mu))) {
	    warning("Step size truncated: out of bounds.")
	    ii <- 1
	    while (!(valideta(eta) && validmu(mu))) {
		if (ii > control$maxit)
		    stop("inner loop 2; can't correct step size")
		ii <- ii + 1
		start <- (start + coefold)/2
		eta[good] <-
		    if (nvars == 1) x[good] * start else as.vector(x[good, ] %*% start)
		mu <- linkinv(eta <- eta + offset)
	    }
	    boundary <- TRUE
	    coef <- start
	    dev <- sum(dev.resids(y, mu, weights))
	    if (control$trace)
		cat("New Deviance =", dev, "\n")
	}
	## check for convergence
	if (abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
	    conv <- TRUE
	    break
	} else {
	    devold <- dev
	    coefold <- coef
	}
    }##-------------- end IRLS iteration -------------------------------

    if (!conv) warning("Algorithm did not converge")
    if (boundary) warning("Algorithm stopped at boundary value")
    ## If X matrix was not full rank then columns were pivoted,
    ## hence we need to re-label the names ...
    ## Original code changed as suggested by BDR---give NA rather
    ## than 0 for non-estimable parameters
    if (fit$rank != nvars) {
	coef[seq(fit$rank+1, nvars)] <- NA
	dimnames(fit$qr) <- list(NULL, xnames)
    }
    coef[fit$pivot] <- coef
    xxnames <- xnames[fit$pivot]
    residuals <- rep(NA, nobs)
    residuals[good] <- z - (eta-offset)[good] # z does not have offset in.
    fit$qr <- as.matrix(fit$qr)
    nr <- min(sum(good), nvars)
    if (nr < nvars) {
	Rmat <- diag(nvars)
	Rmat[1:nr,1:nvars] <- fit$qr[1:nr,1:nvars]
    }
    else Rmat <- fit$qr[1:nvars, 1:nvars]
    Rmat <- as.matrix(Rmat)
    Rmat[row(Rmat) > col(Rmat)] <- 0
    names(coef) <- xnames
    colnames(fit$qr) <- xxnames
    dimnames(Rmat) <- list(xxnames, xxnames)
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    names(w) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    names(fit$effects) <-
	c(xxnames[seq(fit$rank)], rep("", nobs - fit$rank))
    ## calculate null deviance
    wtdmu <-
	if (intercept) sum(weights * y)/sum(weights) else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    ## calculate df
    n.ok <- nobs - sum(weights==0)
    nulldf <- n.ok - as.integer(intercept)
    resdf  <- n.ok - fit$rank
    ## calculate AIC
    aic.model <-
	##Should not be necessary: --pd
	##if(resdf>0) aic(y, n, mu, weights, dev) + 2*fit$rank else -Inf
	aic(y, n, mu, weights, dev) + 2*fit$rank
    list(coefficients = coef, residuals = residuals, fitted.values = mu,
	 effects = fit$effects, R = Rmat, rank = fit$rank,
	 qr = fit[c("qr", "rank", "qraux", "pivot", "tol")], family = family,
	 linear.predictors = eta, deviance = dev, aic = aic.model,
	 null.deviance = nulldev, iter = iter, weights = w^2,
	 prior.weights = weights, df.residual = resdf, df.null = nulldf,
	 y = y, converged = conv, boundary = boundary)
}


print.glm <- function (x, digits= max(3, .Options$digits - 3), na.print="", ...)
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("Coefficients")
    if(is.character(co <- x$contrasts))
	cat("  [contrasts: ",
	    apply(cbind(names(co),co), 1, paste, collapse="="), "]")
    cat(":\n")
    print.default(format(x$coefficients, digits=digits),
		  print.gap = 2, quote = FALSE)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
	x$df.residual, "Residual\n")
    cat("Null Deviance:	   ",   format(signif(x$null.deviance, digits)),
        "\nResidual Deviance:", format(signif(x$deviance, digits)),
        "\tAIC:", format(signif(x$aic, digits)), "\n")
    invisible(x)
}


anova.glm <- function(object, ..., test=NULL, na.action=na.omit)
{
    ## check for multiple objects
    dotargs <- list(...)
    named <- if (is.null(names(dotargs)))
	rep(FALSE,length(dotargs)) else (names(dotargs) != "")
    if(any(named))
	warning(paste("The following arguments to anova.glm(..)",
		      "are invalid and dropped:",
		      paste(deparse(dotargs[named]), collapse=", ")))
    dotargs <- dotargs[!named]
    is.glm <- unlist(lapply(dotargs,function(x) inherits(x,"glm")))
    dotargs <- dotargs[is.glm]
    if (length(dotargs)>0)
	return(anova.glmlist(c(list(object),dotargs),test=test,
			     na.action=na.action))
    ##args <- function(...) nargs()
    ##if(args(...)) return(anova.glmlist(list(object, ...), test=test))

    ## extract variables from model

    varlist <- attr(object$terms, "variables")
    ## must avoid partial matching here.
    x <-
	if (n <- match("x", names(object), 0))
	    object[[n]]
	else model.matrix(object)
    varseq <- attr(x, "assign")
    nvars <- max(varseq)
    resdev <- resdf <- NULL

    ## if there is more than one explanatory variable then
    ## recall glm.fit to fit variables sequentially

    if(nvars > 1) {
	method <- object$method
	if(!is.function(method))
	    method <- get(method, mode = "function")
	for(i in 1:(nvars-1)) {
	    ## explanatory variables up to i are kept in the model
	    ## use method from glm to find residual deviance
	    ## and df for each sequential fit
	    fit <- method(x=x[, varseq <= i],
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

    table <- data.frame(c(NA, -diff(resdf)), c(NA, -diff(resdev)), resdf, resdev)
    if (nvars == 0) table <- table[1,,drop=FALSE] # kludge for null model
    dimnames(table) <- list(c("NULL", attr(object$terms, "term.labels")),
			    c("Df", "Deviance", "Resid. Df", "Resid. Dev"))
    title <- paste("Analysis of Deviance Table", "\n\nModel: ",
		   object$family$family, ", link: ", object$family$link,
		   "\n\nResponse: ", as.character(varlist[-1])[1],
		   "\n\nTerms added sequentially (first to last)\n\n", sep="")

    ## calculate test statistics if needed

    if(!is.null(test))
	table <- stat.anova(table=table, test=test,
                            scale=sum(object$weights*object$residuals^2)/
                            object$df.residual,
			    df.scale=object$df.residual, n=NROW(x))
    structure(table, heading = title, class= c("anova", "data.frame"))
}


anova.glmlist <- function(object, test=NULL, na.action=na.omit)
{

    ## find responses for all models and remove
    ## any models with a different response

    responses <- as.character(lapply(object, function(x) {
	deparse(formula(x)[[2]])} ))
    sameresp <- responses==responses[1]
    if(!all(sameresp)) {
	object <- object[sameresp]
	warning(paste("Models with response", deparse(responses[!sameresp]),
                      "removed because response differs from",
		      "model 1"))
    }

    ## calculate the number of models

    nmodels <- length(object)
    if(nmodels==1)
        return(anova.glm(object[[1]], na.action=na.action, test=test))

    ## extract statistics

    resdf  <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)), c(NA, -diff(resdev)))
    variables <- as.character(lapply(object, function(x) {
	deparse(formula(x)[[3]])} ))
    dimnames(table) <- list(variables, c("Resid. Df", "Resid. Dev", "Df",
					 "Deviance"))
    title <- paste("Analysis of Deviance Table \n\nResponse: ", responses[1],
		   "\n\n", sep="")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- object[[(order(resdf)[1])]]
	table <- stat.anova(table=table, test=test,
                            scale=sum(bigmodel$weights * bigmodel$residuals^2)/
			    bigmodel$df.residual, df.scale=min(resdf),
			    n=length(bigmodel$residuals))
    }
    structure(table, heading = title, class= c("anova", "data.frame"))
}


stat.anova <- function(table, test=c("Chisq", "F", "Cp"), scale, df.scale, n)
{
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if(is.na(dev.col)) dev.col <- match("Sum of Sq", colnames(table))
    switch(test,
	   "Chisq" = {
	       cbind(table,"P(>|Chi|)"= 1-pchisq(abs(table[, dev.col]),
                             abs(table[, "Df"])))
	   },
	   "F" = {
	       Fvalue <- abs((table[, dev.col]/table[, "Df"])/scale)
	       cbind(table, F = Fvalue,
		     "Pr(>F)" = 1-pf(Fvalue, abs(table[, "Df"]), abs(df.scale)))
	   },
	   "Cp" = {
	       cbind(table, Cp = table[,"Resid. Dev"] +
                     2*scale*(n - table[,"Resid. Df"]))
	   })
}

summary.glm <- function(object, dispersion = NULL,
			correlation = FALSE, na.action=na.omit, ...)
{
    Qr <- .Alias(object$qr)
    est.disp <- FALSE
    df.r <- object$df.residual
    if(is.null(dispersion))	# calculate dispersion if needed
	dispersion <-
	    if(any(object$family$family == c("poisson", "binomial")))
		1
	    else if(df.r > 0) {
		est.disp <- TRUE
		if(any(object$weights==0))
		    warning(paste("observations with zero weight",
				  "not used for calculating dispersion"))
		sum(object$weights*object$residuals^2)/ df.r
	    } else Inf

    ## calculate scaled and unscaled covariance matrix

    p <- object$rank
    p1 <- 1:p

    ## WATCHIT! doesn't this rely on pivoting not permuting 1:p?
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
    ## return answer

    ans <- c(object[c("call","terms","family","deviance", "aic",
		      "contrasts",
		      "df.residual","null.deviance","df.null","iter")],
	     list(deviance.resid= residuals(object, type = "deviance"),
		  aic = object$aic,
		  coefficients=coef.table,
		  dispersion=dispersion,
		  df=c(object$rank, df.r),
		  cov.unscaled=covmat.unscaled,
		  cov.scaled=covmat))

    if(correlation) {
	dd <- sqrt(diag(covmat.unscaled))
	ans$correlation <-
	    covmat.unscaled/outer(dd,dd)
    }
    class(ans) <- "summary.glm"
    return(ans)
}

print.summary.glm <- function (x, digits = max(3, .Options$digits - 3),
			       na.print = "", symbolic.cor = p > 4,
			       signif.stars= .Options$show.signif.stars, ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
    cat("Deviance Residuals: \n")
    if(x$df.residual > 5) {
	x$deviance.resid <- quantile(x$deviance.resid,na.rm=TRUE)
	names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    print.default(x$deviance.resid, digits=digits, na = "", print.gap = 2)

    cat("\nCoefficients:\n")
    print.coefmat(x$coef, digits=digits, signif.stars=signif.stars, ...)
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
	"\n\n", sep="")

    correl <- x$correlation
    if(!is.null(correl)) {
	p <- dim(correl)[2]
	if(p > 1) {
	    cat("Correlation of Coefficients:\n")
	    correl[!lower.tri(correl)] <- NA
	    print(correl[-1, -NCOL(correl), drop=FALSE],
		  digits=digits, na="")
	}
	cat("\n")
    }
    invisible(x)
}


## GLM Methods for Generic Functions :

coef.glm <- function(object, ...) object$coefficients
deviance.glm <- function(object, ...) object$deviance
effects.glm <- function(object, ...) object$effects
fitted.glm <- function(object, ...) object$fitted.values

family.glm <- function(object, ...) object$family

residuals.glm <- function(object, type="deviance", ...)
{
    ntyp <- match(type, c("deviance", "pearson", "working", "response", "partial"))
    if(is.na(ntyp))
	stop(paste("invalid `type':", type))
    y  <- object$y
    mu <- object$fitted.values
    wts <- object$prior.weights
    switch(ntyp,
	   deviance = if(object$df.res > 0) {
	       d.res <- sqrt((object$family$dev.resids)(y, mu, wts))
	       ifelse(y > mu, d.res, -d.res)
	   } else rep(0, length(mu)),
	   pearson	 = object$residuals * sqrt(object$weights),
	   working	 = object$residuals,
	   response = y - mu,
           partial=object$residuals+predict(object,type="terms")
	   )
}

## Commented by KH on 1998/06/22
## update.default() should be more general now ...
##update.glm <- function (glm.obj, formula, data, weights, subset, na.action,
##			offset, family, x)
##{
##	call <- glm.obj$call
##	if (!missing(formula))
##	  call$formula <- update.formula(call$formula, formula)
##	if (!missing(data))	call$data <- substitute(data)
##	if (!missing(subset))	call$subset <- substitute(subset)
##	if (!missing(na.action))call$na.action <- substitute(na.action)
##	if (!missing(weights))	call$weights <- substitute(weights)
##	if (!missing(offset))	call$offset <- substitute(offset)
##	if (!missing(family))	call$family <- substitute(family)
##	if (!missing(x))	call$x <- substitute(x)
####	notparent <- c("NextMethod", "update", methods(update))
####	for (i in 1:(1+sys.parent())) {
####		parent <- sys.call(-i)[[1]]
####		if (is.null(parent))
####		break
####	if (is.na(match(as.character(parent), notparent)))
####			break
####	}
####	eval(call, sys.frame(-i))
##	eval(call, sys.frame(sys.parent()))
##}

model.frame.glm <-
    function (formula, data, na.action, ...)
{
    if (is.null(formula$model)) {
	fcall <- formula$call
	fcall$method <- "model.frame"
	fcall[[1]] <- as.name("glm")
	eval(fcall, sys.frame(sys.parent()))
    }
    else formula$model
}
