lm <- function (formula, data = list(), subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset = NULL, ...)
{
    ret.x <- x
    ret.y <- y
    mt <- terms(formula, data = data)
    mf <- match.call()
    mf$singular.ok <- mf$model <- mf$method <- NULL
    mf$x <- mf$y <- mf$qr <- mf$contrasts <- NULL
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, sys.frame(sys.parent()))
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
    } else {
	x <- model.matrix(mt, mf, contrasts)
	z <- if(is.null(w)) lm.fit(x, y, offset=offset)
	else lm.wfit(x, y, w, offset=offset)
	class(z) <- c(if(is.matrix(y)) "mlm", "lm")
    }
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- xlev
    z$call <- match.call()
    z$terms <- mt
    if (model)
	z$model <- mf
    if (ret.x)
	z$x <- x
    if (ret.y)
	z$y <- y
    z
}

lm.fit <- function (x, y, offset = NULL, method = "qr", tol = 1e-07, ...)
{
    if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if (is.null(offset)) offset <- rep(0, NROW(y))
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        cc <- match.call()
        cc[[1]] <- as.name("lm.fit.null")
        return(eval(cc, sys.frame(sys.parent())))
    }
    ny <- NCOL(y)
    ## treat one-col matrix as vector
    if ( is.matrix(y) && ny == 1 ) y <- drop(y)
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
		  pivot = 1:p, qraux = double(p), work = double(2*p))
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- 1:z$rank
    dn <- colnames(x)
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
    c(z[c("coefficients", "residuals", "effects", "rank")],
      list(fitted.values= y + offset - z$residuals, assign= attr(x, "assign"),
	   qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
	   df.residual = n - z$rank))
}

lm.wfit <- function (x, y, w, offset = NULL, method = "qr", tol = 1e-7, ...)
{
    if(is.null(n <- nrow(x))) stop("'x' must be a matrix")
    ny <- NCOL(y)
    if (is.null(offset)) offset <- rep(0, NROW(y))
    ## treat one-col matrix as vector
    if ( is.matrix(y) && ny == 1 ) y <- drop(y)
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
    y <- y - offset
    zero.weights <- any(w == 0)
    if (zero.weights) {
	save.r <- y
	save.f <- y
	save.w <- w
	ok <- w != 0
	nok <- !ok
	w <- w[ok]
	x0 <- x[!ok, ]
	x <- x[ok, ]
	n <- nrow(x)
	y0 <- if (ny > 1) y[!ok, , drop = FALSE] else y[!ok]
	y  <- if (ny > 1) y[ ok, , drop = FALSE] else y[ok]
    }
    p <- ncol(x)
    if (p == 0) {
        ## oops, null model
        cc <- match.call()
        cc[[1]] <- as.name("lm.wfit.null")
        return(eval(cc, sys.frame(sys.parent())))
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
		  work = double(2 * p))
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- 1:z$rank
    dn <- colnames(x)
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
    z$fitted.values <- (y - z$residuals)
    z$weights <- w
    if (zero.weights) {
	coef[is.na(coef)] <- 0
	f0 <- x0 %*% coef
	if (ny > 1) {
	    save.r[ok, ] <- z$residuals
	    save.r[nok, ] <- y0 - f0
	    save.f[ok, ] <- z$fitted.values + offset[ok,]
	    save.f[nok, ] <- f0 + offset[nok,]
	}
	else {
	    save.r[ok] <- z$residuals
	    save.r[nok] <- y0 - f0
	    save.f[ok] <- z$fitted.values + offset[ok]
	    save.f[nok] <- f0 + offset[nok]
	}
	z$residuals <- save.r
	z$fitted.values <- save.f
	z$weights <- save.w
    } else
        z$fitted.values <- z$fitted.values + offset
    c(z[c("coefficients", "residuals", "fitted.values", "effects",
	  "weights", "rank")],
      list(assign = attr(x, "assign"),
	   qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
	   df.residual = n - z$rank))
}

print.lm <- function(x, digits = max(3, .Options$digits - 3), ...)
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
    n <- NROW(Qr$qr)
    p <- z$rank
    rdf <- n - p
    p1 <- 1:p
    r <- resid(z)
    f <- fitted(z)
    w <- weights(z)
    if (is.null(z$terms)) {
	stop("invalid \'lm\' object:  no terms component")
    } else {
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
    function (x, digits = max(3, .Options$digits - 3), symbolic.cor = p > 4,
	      signif.stars= .Options$show.signif.stars,	...)
{
    cat("\nCall:\n")#S: ' ' instead of '\n'
    cat(paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep="")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    cat("Residuals:\n")
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
	cat("ALL",df[1],"residuals are 0: no residual degrees of freedom!\n")
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
	p <- dim(correl)[2]
	if (p > 1) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(symbolic.cor)
		print(symnum(correl)[-1,-p])
	    else {
		correl[!lower.tri(correl)] <- NA
		print(correl[-1, -p],
		      digits = digits, na = "")
	    }
	}
    }
    cat("\n")#- not in S
    invisible(x)
}

## Commented by KH on 1998/07/10
## update.default() should be more general now ...
## update.lm <- function(lm.obj, formula, data, weights, subset, na.action)
## .....

residuals.lm <- function(object, ...) object$residuals
fitted.lm <- function(object, ...) object$fitted.values
coef.lm <- function(object, ...) object$coefficients
## need this for results of lm.fit() in drop1():
weights.default <- function(object, ...) object$weights
weights.lm <- .Alias(weights.default)
df.residual.lm <- function(object, ...) object$df.residual
deviance.lm <- function(object, ...) sum(weighted.residuals(object)^2)
formula.lm <- function(object, ...) formula(object$terms)
family.lm <- function(object, ...) { gaussian() }

model.frame.lm <-
    function(formula, data, na.action, ...) {
	if (is.null(formula$model)) {
	    fcall <- formula$call
	    fcall$method <- "model.frame"
	    fcall[[1]] <- as.name("lm")
	    eval(fcall, sys.frame(sys.parent()))
	}
	else formula$model
    }

variable.names.lm <- function(obj, full=FALSE)
{
    if(full)	dimnames(obj$qr$qr)[[2]]
    else	dimnames(obj$qr$qr)[[2]][1:obj$rank]
}

case.names.lm <- function(obj, full=FALSE)
{
    w <- weights(obj)
    dn <- .Alias(names(obj$residuals))
    if(full || is.null(w)) dn else dn[w!=0]
}

anova.lm <- function(object, ...)
{
    if(length(list(object, ...)) > 1)
	return(anovalist.lm(object, ...))
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

predict.lm <- function(object, newdata,
		       se.fit = FALSE, scale = NULL, df = Inf,
		       interval=c("none","confidence","prediction"),
                       level=.95, ...)
{
    if(missing(newdata)) {
        X <- model.matrix(object)
        offset <- object$offset
    }
    else {
        tt <- terms(object)
	X <- model.matrix(delete.response(tt), newdata,
			  contrasts = object$contrasts, xlev = object$xlevels)
	offset <- if (!is.null(off.num<-attr(tt,"offset")))
	    eval(attr(tt,"variables")[[off.num+1]], newdata)
	else if (!is.null(object$offset))
	    eval(object$call$offset, newdata)
    }
    n <- NROW(object$qr$qr)
    p <- object$rank
    p1 <- 1:p
    piv <- object$qr$pivot[p1]
    est <- object$coefficients[piv]
    predictor <- drop(X[, piv, drop = FALSE] %*% est)
    if ( !is.null(offset) ) predictor <- predictor + offset
    interval <- match.arg(interval)
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
	R <- chol2inv(object$qr$qr[p1, p1, drop = FALSE])
	vcov <- res.var * R
	ip <- real(NROW(X))
	for (i in (1:NROW(X))) {
	    xi <- X[i, piv]
	    ip[i] <- xi %*% vcov %*% xi
	}
    }
    if(interval != "none") {
	tfrac <- qt((1 - level)/2,df)
	w <- tfrac * switch(interval,
			    confidence=sqrt(ip),
			    prediction=sqrt(ip+res.var)
			    )
	predictor <- cbind(predictor, predictor + w %o% c(1,-1))
	colnames(predictor) <- c("fit","lwr","upr")
    }
    if(se.fit)
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
