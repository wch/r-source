lm <- function (formula, data = list(), subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset = NULL, ...)
{
    ret.x <- x
    ret.y <- y
    mt <- terms(formula, data = data)
    mf <- cl <- match.call()
    mf$singular.ok <- mf$model <- mf$method <- NULL
    mf$x <- mf$y <- mf$qr <- mf$contrasts <- NULL
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if (method == "model.frame")
	return(mf)
    else if (method != "qr")
	warning(paste("method =", method,
		      "is not supported. Using \"qr\"."))
    xvars <- as.character(attr(mt, "variables"))[-1]
    if((yvar <- attr(mt, "response")) > 0) xvars <- xvars[-yvar]
    xlev <-
	if(length(xvars) > 0) {
	    xlev <- lapply(mf[xvars], levels)
	    xlev[!sapply(xlev, is.null)]
	}
    if (length(list(...)))
	warning(paste("Extra arguments", deparse(substitute(...)),
		      "are just disregarded."))
    if (!singular.ok)
	warning("only `singular.ok = TRUE' is currently implemented.")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    offset <- model.offset(mf)
    if(!is.null(offset) && length(offset) != NROW(y))
	stop(paste("Number of offsets is", length(offset),
		   ", should equal", NROW(y), "(number of observations)"))

    if (is.empty.model(mt)) {
	x <- NULL
	z <- list(coefficients = numeric(0), residuals = y,
		  fitted.values = 0 * y + offset, weights = w, rank = 0,
		  df.residual = length(y))
	class(z) <-
	    if (is.matrix(y))
		c("mlm.null", "lm.null", "mlm", "lm")
	    else c("lm.null", "lm")
    }
    else {
	x <- model.matrix(mt, mf, contrasts)
	z <- if(is.null(w)) lm.fit(x, y, offset=offset)
	else lm.wfit(x, y, w, offset=offset)
	class(z) <- c(if(is.matrix(y)) "mlm", "lm")
    }
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- xlev
    z$call <- cl
    z$terms <- mt
    if (model)
	z$model <- mf
    if (ret.x)
	z$x <- x
    if (ret.y)
	z$y <- y
    z
}

## lm.fit() and lm.wfit() have *MUCH* in common  [say ``code re-use !'']
lm.fit <- function (x, y, offset = NULL, method = "qr", tol = 1e-07, ...)
{
    if (is.null(n <- nrow(x))) stop("`x' must be a matrix")
    if(n == 0) stop("0 (non-NA) cases")
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        cc <- match.call()
        cc[[1]] <- as.name("lm.fit.null")
        return(eval(cc, parent.frame()))
    }
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n)
	stop("incompatible dimensions")
    if(method != "qr")
	warning(paste("method =",method,
		      "is not supported. Using \"qr\"."))
    if(length(list(...)))
	warning(paste("Extra arguments", deparse(substitute(...)),
		      "are just disregarded."))
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .Fortran("dqrls",
		  qr = x, n = n, p = p,
		  y = y, ny = ny,
		  tol = as.double(tol),
		  coefficients = mat.or.vec(p, ny),
		  residuals = y, effects = y, rank = integer(1),
		  pivot = 1:p, qraux = double(p), work = double(2*p),
                  PACKAGE="base")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- 1:z$rank
    dn <- colnames(x); if(is.null(dn)) dn <- paste("x", 1:p, sep="")
    nmeffects <- c(dn[pivot[r1]], rep("", n - z$rank))
    if (is.matrix(y)) {
	coef[-r1, ] <- NA
	coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects,colnames(y))
    } else {
	coef[-r1] <- NA
	coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    r1 <- y - z$residuals ; if(!is.null(offset)) r1 <- r1 + offset
    c(z[c("coefficients", "residuals", "effects", "rank")],
      list(fitted.values = r1, assign = attr(x, "assign"),
	   qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
	   df.residual = n - z$rank))
}

lm.wfit <- function (x, y, w, offset = NULL, method = "qr", tol = 1e-7, ...)
{
    if(is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if(n == 0) stop("0 (non-NA) cases")
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if(is.matrix(y) && ny == 1)
        y <- drop(y)
    if(!is.null(offset))
        y <- y - offset
    if (NROW(y) != n | length(w) != n)
	stop("incompatible dimensions")
    if (any(w < 0 | is.na(w)))
	stop("missing or negative weights not allowed")
    if(method != "qr")
	warning(paste("method =",method,
		      "is not supported. Using \"qr\"."))
    if(length(list(...)))
	warning(paste("Extra arguments", deparse(substitute(...)),
		      "are just disregarded."))
    x.asgn <- attr(x, "assign")# save
    zero.weights <- any(w == 0)
    if (zero.weights) {
	save.r <- y
	save.f <- y
	save.w <- w
	ok <- w != 0
	nok <- !ok
	w <- w[ok]
	x0 <- x[!ok, , drop = FALSE]
	x <- x[ok,  , drop = FALSE]
	n <- nrow(x)
	y0 <- if (ny > 1) y[!ok, , drop = FALSE] else y[!ok]
	y  <- if (ny > 1) y[ ok, , drop = FALSE] else y[ok]
    }
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        cc <- match.call()
        cc[[1]] <- as.name("lm.wfit.null")
        return(eval(cc, parent.frame()))
    }
    storage.mode(y) <- "double"
    wts <- sqrt(w)
    z <- .Fortran("dqrls",
		  qr = x * wts, n = n, p = p,
		  y  = y * wts, ny = ny,
		  tol = as.double(tol),
		  coefficients = mat.or.vec(p, ny), residuals = y,
		  effects = mat.or.vec(n, ny),
		  rank = integer(1), pivot = 1:p, qraux = double(p),
		  work = double(2 * p),
                  PACKAGE="base")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- 1:z$rank
    dn <- colnames(x); if(is.null(dn)) dn <- paste("x", 1:p, sep="")
    nmeffects <- c(dn[pivot[r1]], rep("", n - z$rank))
    if (is.matrix(y)) {
	coef[-r1, ] <- NA
	coef[pivot, ] <- coef
	dimnames(coef) <- list(dn, colnames(y))
	dimnames(z$effects) <- list(nmeffects,colnames(y))
    } else {
	coef[-r1] <- NA
	coef[pivot] <- coef
	names(coef) <- dn
	names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    z$residuals <- z$residuals/wts
    z$fitted.values <- y - z$residuals
    z$weights <- w
    if (zero.weights) {
	coef[is.na(coef)] <- 0
	f0 <- x0 %*% coef
	if (ny > 1) {
	    save.r[ok, ] <- z$residuals
	    save.r[nok, ] <- y0 - f0
	    save.f[ok, ] <- z$fitted.values
	    save.f[nok, ] <- f0
	}
	else {
	    save.r[ok] <- z$residuals
	    save.r[nok] <- y0 - f0
	    save.f[ok] <- z$fitted.values
	    save.f[nok] <- f0
	}
	z$residuals <- save.r
	z$fitted.values <- save.f
	z$weights <- save.w
    }
    if(!is.null(offset))
        z$fitted.values <- z$fitted.values + offset
    c(z[c("coefficients", "residuals", "fitted.values", "effects",
	  "weights", "rank")],
      list(assign = x.asgn,
	   qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
	   df.residual = n - z$rank))
}

print.lm <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n",deparse(x$call),"\n\n",sep="")
    cat("Coefficients:\n")
    print.default(format(coef(x), digits=digits),
		  print.gap = 2, quote = FALSE)
    cat("\n")
    invisible(x)
}

summary.lm <- function (object, correlation = FALSE, ...)
{
    z <- .Alias(object)
    Qr <- .Alias(object$qr)
    if (is.null(z$terms) || is.null(Qr))
	stop("invalid \'lm\' object:  no terms or qr component")
    n <- NROW(Qr$qr)
    p <- z$rank
    rdf <- n - p
    if(rdf != z$df.residual)
        warning("inconsistent residual degrees of freedom. -- please report!")
    p1 <- 1:p
    r <- resid(z)
    f <- fitted(z)
    w <- weights(z)
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept"))
            sum((f - mean(f))^2) else sum(f^2)
        rss <- sum(r^2)
    } else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f /sum(w))
            sum(w * (f - m)^2)
        } else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms")]
    ans$residuals <- r
    ans$coefficients <- cbind(est, se, tval, 2*(1 - pt(abs(tval), rdf)))
    dimnames(ans$coefficients)<-
	list(names(z$coefficients)[Qr$pivot[p1]],
	     c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
	df.int <- if (attr(z$terms, "intercept")) 1 else 0
	ans$r.squared <- mss/(mss + rss)
	ans$adj.r.squared <- 1 - (1 - ans$r.squared) *
	    ((n - df.int)/rdf)
	ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
			    numdf = p - df.int, dendf = rdf)
    }
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,1)]
    if (correlation) {
	ans$correlation <- (R * resvar)/outer(se, se)
	dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    }
    class(ans) <- "summary.lm"
    ans
}

print.summary.lm <-
    function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p > 4,
	      signif.stars= getOption("show.signif.stars"),	...)
{
    cat("\nCall:\n")#S: ' ' instead of '\n'
    cat(paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep="")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    cat(if(!is.null(x$w) && diff(range(x$w))) "Weighted ",
        "Residuals:\n", sep="")
    if (rdf > 5) {
	nam <- c("Min", "1Q", "Median", "3Q", "Max")
	rq <- if (length(dim(resid)) == 2)
	    structure(apply(t(resid), 1, quantile),
		      dimnames = list(nam, dimnames(resid)[[2]]))
	else  structure(quantile(resid), names = nam)
	print(rq, digits = digits, ...)
    }
    else if (rdf > 0) {
	print(resid, digits = digits, ...)
    } else { # rdf == 0 : perfect fit!
	cat("ALL", df[1], "residuals are 0: no residual degrees of freedom!\n")
    }
    if (nsingular <- df[3] - df[1])
	cat("\nCoefficients: (", nsingular,
	    " not defined because of singularities)\n", sep = "")
    else cat("\nCoefficients:\n")

    print.coefmat(x$coef, digits=digits, signif.stars=signif.stars, ...)
    ##
    cat("\nResidual standard error:",
	format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom\n")
    if (!is.null(x$fstatistic)) {
	cat("Multiple R-Squared:", formatC(x$r.squared, digits=digits))
	cat(",\tAdjusted R-squared:",formatC(x$adj.r.squared,d=digits),
	    "\nF-statistic:", formatC(x$fstatistic[1], digits=digits),
	    "on", x$fstatistic[2], "and",
	    x$fstatistic[3], "degrees of freedom,\tp-value:",
	    formatC(1 - pf(x$fstatistic[1], x$fstatistic[2],
			   x$fstatistic[3]), dig=digits),
	    "\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
	p <- NCOL(correl)
	if (p > 1) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(symbolic.cor)
		print(symnum(correl)[-1,-p])
	    else {
		correl[!lower.tri(correl)] <- NA
		print(correl[-1, -p, drop=FALSE],
		      digits = digits, na = "")
	    }
	}
    }
    cat("\n")#- not in S
    invisible(x)
}

## KH on 1998/07/10: update.default() is now used ...

residuals.lm <-
    function(object,
             type = c("working","response", "deviance","pearson", "partial"),
             ...)
{
    type <- match.arg(type)
    r <- .Alias(object$residuals)
    switch(type,
           working =, response = r,
           deviance=,
           pearson =if(is.null(object$weights)) r else r * sqrt(object$weights),
	   partial = r + predict(object,type="terms")
           )
}
fitted.lm <- function(object, ...) object$fitted.values
coef.lm <- function(object, ...) object$coefficients
## need this for results of lm.fit() in drop1():
weights.default <- function(object, ...) object$weights
weights.lm <- .Alias(weights.default)
df.residual.lm <- function(object, ...) object$df.residual
deviance.lm <- function(object, ...) sum(weighted.residuals(object)^2)
formula.lm <- function(object, ...) formula(object$terms)
family.lm <- function(object, ...) { gaussian() }

model.frame.lm <- function(formula, data, na.action, ...) {
    if (is.null(formula$model)) {
        fcall <- formula$call
        fcall$method <- "model.frame"
        fcall[[1]] <- as.name("lm")
	env<-environment(fcall$formula)
	if (is.null(env)) env<-parent.frame()
        eval(fcall, env)
    }
    else formula$model
}

variable.names.lm <- function(object, full=FALSE)
{
    if(full)	dimnames(object$qr$qr)[[2]]
    else	dimnames(object$qr$qr)[[2]][1:object$rank]
}

case.names.lm <- function(object, full=FALSE)
{
    w <- weights(object)
    dn <- .Alias(names(object$residuals))
    if(full || is.null(w)) dn else dn[w!=0]
}

anova.lm <- function(object, ...)
{
    if(length(list(object, ...)) > 1)
	return(anova.lmlist(object, ...))
    w <- weights(object)
    ssr <- sum(if(is.null(w)) resid(object)^2 else w*resid(object)^2)
    p1 <- 1:object$rank
    comp <- object$effects[p1]
    asgn <- object$assign[object$qr$pivot][p1]
    nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
    tlabels <- nmeffects[1 + unique(asgn)]
    ss <- c(unlist(lapply(split(comp^2,asgn), sum)), ssr)
    dfr <- df.residual(object)
    df <- c(unlist(lapply(split(asgn,  asgn), length)), dfr)
    ms <- ss/df
    f <- ms/(ssr/dfr)
    p <- 1 - pf(f,df,dfr)
    table <- data.frame(df,ss,ms,f,p)
    table[length(p),4:5] <- NA
    dimnames(table) <- list(c(tlabels, "Residuals"),
			    c("Df","Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
    if(attr(object$terms,"intercept")) table <- table[-1, ]
    structure(table, heading = c("Analysis of Variance Table\n",
		     paste("Response:", deparse(formula(object)[[2]]))),
	      class= c("anova", "data.frame"))# was "tabular"
}

anova.lmlist <- function (object, ..., scale = 0, test = "F")
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) deparse(x$terms[[2]])))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning(paste("Models with response",
		      deparse(responses[!sameresp]),
		      "removed because response differs from", "model 1"))
    }

    ns <- sapply(objects, function(x) length(x$residuals))
    if(any(ns != ns[1]))
        stop("models were not all fitted to the same size of dataset")

    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.lm(object))

    ## extract statistics

    resdf  <- as.numeric(lapply(objects, df.residual))
    resdev <- as.numeric(lapply(objects, deviance))

    ## construct table and title

    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)),
                        c(NA, -diff(resdev)) )
    variables <- lapply(objects, function(x)
                        paste(deparse(formula(x)), collapse="\n") )
    dimnames(table) <- list(1:nmodels,
                            c("Res.Df", "RSS", "Df", "Sum of Sq"))

    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     variables, sep="", collapse="\n")

    ## calculate test statistic if needed

    if(!is.null(test)) {
	bigmodel <- order(resdf)[1]
        scale <- if(scale > 0) scale else resdev[bigmodel]/resdf[bigmodel]
	table <- stat.anova(table = table, test = test,
			    scale = scale,
                            df.scale = resdf[bigmodel],
			    n = length(objects[bigmodel$residuals]))
    }
    structure(table, heading = c(title, topnote),
              class = c("anova", "data.frame"))
}


anovalist.lm <- function (object, ..., test = NULL)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) as.character(x$terms[[2]])))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning(paste("Models with response",
		      deparse(responses[!sameresp]),
		      "removed because response differs from", "model 1"))
    }
    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.lm(object))

    models <- as.character(lapply(objects, function(x) x$terms))

    ## extract statistics
    df.r <- unlist(lapply(objects, df.residual))
    ss.r <- unlist(lapply(objects, deviance))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep(NA,nmodels)
    for(i in 2:nmodels) {
	if(df[i] > 0) {
	    f[i] <- ms[i]/(ss.r[i]/df.r[i])
	    p[i] <- 1 - pf(f[i], df[i], df.r[i])
	}
	else if(df[i] < 0) {
	    f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
	    p[i] <- 1 - pf(f[i], -df[i], df.r[i-1])
	}
	else { # df[i] == 0
	  ss[i] <- 0
	}
    }
    table <- data.frame(df.r,ss.r,df,ss,f,p)
    dimnames(table) <- list(1:nmodels, c("Res.Df", "Res.Sum Sq", "Df",
					 "Sum Sq", "F value", "Pr(>F)"))
    ## construct table and title
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     models, sep="", collapse="\n")

    ## calculate test statistic if needed
    structure(table, heading = c(title, topnote),
	      class= c("anova", "data.frame"))# was "tabular"
}

## code from John Maindonald 26Jul2000
"predict.lm" <- function(object, newdata,
		       se.fit = FALSE, scale = NULL, df = Inf,
		       interval = c("none", "confidence", "prediction"),
                       level = .95,  type = c("response", "terms"),
                       terms = NULL, ...)
{
## june 24 2000 (3 minor changes from JM's May 7 version)
    attrassign <- function (object, ...) UseMethod("attrassign")
    attrassign.lm <- function (lmobj)
        attrassign(model.matrix(lmobj), terms(lmobj))
    attrassign.default <- function (mmat, tt) {
      if (!inherits(tt, "terms"))
        stop("need terms object")
      aa <- attr(mmat, "assign")
      if (is.null(aa))
        stop("argument is not really a model matrix")
      ll <- attr(tt, "term.labels")
      if (attr(tt, "intercept") > 0)
        ll <- c("(Intercept)", ll)
      aaa <- factor(aa, labels = ll)
      split(order(aa), aaa)
    }
    tt <- terms(object)
    if(missing(newdata)) {
        X <- model.matrix(object)
        offset <- object$offset
    }
    else {
        X <- model.matrix(delete.response(tt), newdata,
			  contrasts = object$contrasts, xlev = object$xlevels)
	offset <- if (!is.null(off.num <- attr(tt, "offset")))
	    eval(attr(tt, "variables")[[off.num+1]], newdata)
	else if (!is.null(object$offset))
	    eval(object$call$offset, newdata)
    }
    n <- NROW(object$qr$qr)
    p <- object$rank
    p1 <- 1:p
    piv <- object$qr$pivot[p1]
## NB: Q[p1,]%*%X[,piv]=R[p1,p1]
    beta <- object$coefficients
    predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
    if ( !is.null(offset) ) predictor <- predictor + offset
    interval <- match.arg(interval)
    type <- match.arg(type)
    if(se.fit || interval != "none") {
	if (is.null(scale)) {
	    r <- resid(object)
	    f <- fitted(object)
	    w <- weights(object)
	    rss <- sum(if(is.null(w)) r^2 else r^2 * w)
	    df <- n - p
	    res.var <- rss/df
	} else {
	    res.var <- scale^2
	}
 ## type!="terms"
    if(type!="terms"){
       if(missing(newdata))
       XRinv <- qr.Q(object$qr)[, p1]
       else {
             Rinv <- qr.solve(qr.R(object$qr)[p1, p1])
             XRinv <- X[, piv]%*%Rinv
             }
	ip <- drop(XRinv^2%*%rep(res.var, p))
	}
    }
## type=="terms"
    if (type=="terms"){
      asgn <- attrassign(object)
      hasintercept <- attr(tt, "intercept")>0
      if (hasintercept){
        asgn$"(Intercept)" <- NULL
        avx <- rep(1/n, n)%*%model.matrix(object)
	termsconst <- sum(avx[piv]*beta[piv])
	}
      nterms <- length(asgn)
      predictor <- matrix(ncol=nterms, nrow=NROW(X))
      dimnames(predictor) <- list(rownames(X), names(asgn))

      if (se.fit||interval!="none"){
        ip <- matrix(ncol=nterms, nrow=NROW(X))
        dimnames(ip) <- list(rownames(X), names(asgn))
	Rinv <- qr.solve(qr.R(object$qr)[p1, p1])
      }
      if(hasintercept)
          X <- sweep(X, 2, avx)
      unpiv <- rep(0, NCOL(X))
      unpiv[piv] <- p1
## Predicted values will be set to 0 for any term that
## corresponds to columns of the X-matrix that are
## completely aliased with earlier columns.
      for (i in seq(1, nterms, length=nterms)){
        iipiv <- asgn[[i]]  # Columns of X, ith term
	ii <- unpiv[iipiv]  # Corresponding rows of Rinv
        iipiv[ii==0] <- 0
	if(any(iipiv)>0)
	        predictor[, i] <- X[, iipiv, drop=FALSE]%*%(beta[iipiv])
		else predictor[, i] <- rep(0, NROW(predictor))
        if (se.fit||interval!="none"){
	  if(any(iipiv)>0)

              ip[, i] <- as.matrix(X[, iipiv, drop=FALSE] %*%
                                  Rinv[ii, , drop=FALSE])^2 %*% rep(res.var, p)
	  else ip[, i] <- rep(0, NROW(ip))
        }
      }

      if (!is.null(terms)){
        predictor <- predictor[, terms, drop=FALSE]
        if (se.fit)
          ip <- ip[, terms, drop=FALSE]
    }
      attr(predictor, 'constant') <- if (hasintercept) termsconst else 0
  }
## Now construct elements of the list that will be returned
    if(interval != "none") {
	tfrac <- qt((1 - level)/2, df)
	w <- tfrac * switch(interval,
			    confidence=sqrt(ip),
			    prediction=sqrt(ip+res.var)
			    )
        if(type!="terms"){
            predictor <- cbind(predictor, predictor + w %o% c(1, -1))
            colnames(predictor) <- c("fit", "lwr", "upr")
        }
        else {
            lwr <- predictor + w
            upr <- predictor - w
        }
    }
    if(type=="terms" && interval!="none")
	list(fit = predictor, se.fit = sqrt(ip), lwr=lwr,upr=upr,
	     df = df, residual.scale = sqrt(res.var))
    else if (se.fit)
              list(fit = predictor, se.fit = sqrt(ip),
	     df = df, residual.scale = sqrt(res.var))
    else predictor
}




effects.lm <- function(object, set.sign = FALSE)
{
    eff <- object$effects
    if(set.sign) {
	dd <- coef(object)
	if(is.matrix(eff)) {
	    r <- 1:dim(dd)[1]
	    eff[r,  ] <- sign(dd) * abs(eff[r,	])
	} else {
	    r <- 1:length(dd)
	    eff[r] <- sign(dd) * abs(eff[r])
	}
    }
    structure(eff, assign = object$assign, class = "coef")
}

## plot.lm --> now in ./plot.lm.R

model.matrix.lm <- function(object, ...)
{
    if(n <- match("x", names(object), 0)) object[[n]]
    else {
	data <- model.frame(object, xlev = object$xlevels, ...)
	NextMethod("model.matrix", data = data, contrasts = object$contrasts)
    }
}

##---> SEE ./mlm.R  for more methods, etc. !!
predict.mlm <- function(object, newdata, se.fit = FALSE, ...)
{
    if(missing(newdata)) return(object$fitted)
    if(se.fit)
	stop("The 'se.fit' argument is not yet implemented for mlm objects")
    x <- model.matrix(object, newdata) # will use model.matrix.lm
    piv <- object$qr$pivot[1:object$rank]
    pred <- X[, piv, drop = FALSE] %*% object$coefficients[piv,]
    if(inherits(object, "mlm")) pred else pred[, 1]
}
