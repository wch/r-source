lm <-
function(formula, data = list(), subset, weights, na.action,
         method = "qr", model = TRUE, x = FALSE, y = FALSE,
         qr = TRUE, singular.ok = TRUE, ...)
{
        ret.x <- x
        ret.y <- y
        mt <- terms(formula, data = data)
	mf <- match.call()
	mf$singular.ok <- NULL
	mf$model <- NULL
	mf$method <- NULL
        mf$x <- mf$y <- mf$qr <- NULL
	mf[[1]] <- as.name("model.frame")
	mf <- eval(mf, sys.frame(sys.parent()))
	if (method == "model.frame")
		return(mf)
	else if (method != "qr")
		warning(paste("method =", method,
			      "is not supported. Using \"qr\"."))
	if (length(list(...)))
		warning(paste("Extra arguments", deparse(substitute(...)),
			"are just disregarded."))
	if (!is.null(model.offset(mf)))
		stop("offset() not available in lm(), use glm()")
	if (!singular.ok)
		warning("only `singular.ok = TRUE' is currently implemented.")
	y <- model.response(mf, "numeric")
	w <- model.weights(mf)
	if (is.empty.model(mt)) {
                x <- NULL
		z <- list(coefficients = numeric(0), residuals = y,
			fitted.values = 0 * y, weights = w, rank = 0,
			df.residual = length(y))
		class(z) <- if (is.matrix(y))
			c("mlm.null", "lm.null", "mlm", "lm")
		else c("lm.null", "lm")
	} else {
		x <- model.matrix(mt, mf)
		z <- if (is.null(w))
			lm.fit(x, y)
		else lm.wfit(x, y, w)
		class(z) <- c(if (is.matrix(y)) "mlm", "lm")
	}
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

lm.fit <- function (x, y, method = "qr", tol = 1e-07, ...)
{
	n <- nrow(x)
	p <- ncol(x)
	ny <- NCOL(y)
	if (NROW(y) != n)
		stop("incompatible dimensions")
	if(method != "qr")
		warning(paste("method =",method,
			      "is not supported. Using \"qr\"."))
	if(length(list(...)))
		warning(paste("Extra arguments", deparse(substitute(...)),
			      "are just disregarded."))
	z <- .Fortran("dqrls", qr = x, n = n, p = p, y = y, ny = ny,
		tol = tol, coefficients = mat.or.vec(p, ny),
		residuals = y, effects = y, rank = integer(1),
		pivot = 1:p, qraux = double(p), work = double(2*p))
	coef <- z$coefficients
	pivot <- z$pivot
	r1 <- 1:z$rank
	if (ny > 1) {
		coef[-r1, ] <- NA
		coef[pivot, ] <- coef
		dimnames(coef) <- list(dimnames(x)[[2]], dimnames(y)[[2]])
		rownames(z$effects) <- NULL
	} else {
		coef[-r1] <- NA
		coef[pivot] <- coef
		names(coef) <- dimnames(x)[[2]]
		names(z$effects) <- NULL
	}
	z$coefficients <- coef
	c(z[c("coefficients", "residuals", "effects", "rank")],
		list(fitted.values= y - z$residuals, assign= attr(x, "assign"),
			qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
			df.residual = n - z$rank))
}

lm.wfit <- function (x, y, w, method = "qr", tol = 1e-7, ...)
{
	n <- nrow(x)
	p <- ncol(x)
	ny <- NCOL(y)
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
		y0 <- if (ny > 1) y[!ok, , drop = FALSE] else y[!ok]
		y  <- if (ny > 1) y[ ok, , drop = FALSE] else y[ok]
	}
	n <- nrow(x)
	p <- ncol(x)
	wts <- w^0.5
	z <- .Fortran("dqrls", qr = x * wts, n = n, p = p, y = y *
		wts, ny = ny, tol = tol, coefficients = mat.or.vec(p,
		ny), residuals = y, effects = mat.or.vec(n, ny),
		rank = integer(1), pivot = 1:p, qraux = double(p),
		work = double(2 * p))
	coef <- z$coefficients
	pivot <- z$pivot
	r1 <- 1:z$rank
	if (ny > 1) {
		coef[-r1, ] <- NA
		coef[pivot, ] <- coef
		dimnames(coef) <- list(dimnames(x)[[2]], dimnames(y)[[2]])
		dimnames(z$residuals) <- dimnames(y)
		dimnames(z$effects)[[2]] <- dimnames(y)[[2]]
	}
	else {
		coef[-r1] <- NA
		coef[pivot] <- coef
		names(coef) <- dimnames(x)[[2]]
		names(z$residuals) <- names(y)
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
	else {
		if (ny > 1) {
			dimnames(z$residuals) <- dimnames(y)
			dimnames(z$fitted.values) <- dimnames(y)
		}
		else {
			names(z$residuals) <- names(y)
			names(z$fitted.values) <- names(y)
		}
	}
	c(z[c("coefficients", "residuals", "fitted.values", "effects",
		"weights", "rank")], list(assign = attr(x, "assign"),
		qr = z[c("qr", "qraux", "pivot", "tol", "rank")],
		df.residual = n - z$rank))
}

print.lm <- function(x, digits = max(3, .Options$digits - 3), ...)
{
	cat("\nCall:\n",deparse(x$call),"\n\n",sep="")
	cat("Coefficients:\n")
	print(coef(x))
	cat("\n")
	invisible(x)
}

summary.lm <- function (object, correlation = FALSE)
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
	dimnames(ans$coefficients)<-list(names(z$coefficients)[Qr$pivot[p1]],
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

print.summary.lm <- function (x, digits = max(3, .Options$digits - 3),
			      symbolic.cor = p > 4, signif.stars= TRUE, ...)
{
	cat("\nCall:\n")#S: ' ' instead of '\n'
	cat(paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep="")
	##0.61: dput(x$call)
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
	}
	if (nsingular <- df[3] - df[1])
		cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
			sep = "")
	else cat("\nCoefficients:\n")
	##O R 0.61:
	##O print(roundfun(x$coefficients, digits = digits), quote = FALSE, ...)
	##- Splus3.{1-4}: Coefs <- format(round(x$coef, digits = digits))
	##- ============   CANNOT be good for funny scales of Y
	acs <- abs(coef.se <- x$coef[, 1:2, drop=FALSE])
	digmin <- 1+floor(log10(range(acs[acs != 0], na.rm= TRUE)))
	## = digits for rounding col 1:2
	digt <- max(1, min(5, digits - 1))
	## You need this, e.g., for   "rlm" class from MASS library:
	has.Pval <- ncol(x$coef)>= 4# or any("Pr(>|t|)" == dimnames(x$coef)[[2]]
	if(has.Pval)
		Pv <- x$coef[, 4]
	Coefs <-
	  cbind(format(round(coef.se, max(1,digits - digmin)), digits=digits),
		format(round(x$coef[, 3], dig=digt), digits=digits),# t- values
		if(has.Pval) format.pval(Pv, digits = digt))
	dimnames(Coefs) <- dimnames(x$coef)
	if(any(not.both.0 <- (c(x$coef)==0)!=(as.numeric(Coefs)==0),na.rm=TRUE))
	  ## not.both.0==T:  one is TRUE, one is FALSE : ==> x$coef != 0
	  Coefs[not.both.0] <- format(x$coef[not.both.0], digits= min(1,digits-1))# =2
	if(!has.Pval || !exists("symnum", mode = "function"))
		signif.stars <- FALSE
	else if(signif.stars) {
		Signif <- symnum(Pv, corr = FALSE,
				 cutpoints = c(0,  .001,.01,.05, .1, 1),
				 symbols   =  c("***","**","*","."," "))
		Coefs <- cbind(Coefs, Signif)
	}
	print(Coefs, quote = FALSE, ...)
	if(signif.stars) cat("---\nSignif. codes: ",attr(Signif,"legend"),"\n")

	cat("\nResidual standard error:", format(signif(x$sigma,
		digits)), "on", rdf, "degrees of freedom\n")
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

update.lm <- function(lm.obj, formula, data, weights, subset, na.action)
{
	call <- lm.obj$call
	if(!missing(formula))
		call$formula <- update.formula(call$formula, formula)
	if(!missing(data))	call$data <- substitute(data)
	if(!missing(subset))	call$subset <- substitute(subset)
	if(!missing(na.action)) call$na.action <- substitute(na.action)
	if (!missing(weights))	call$weights<-substitute(weights)
	eval(call, sys.frame(sys.parent()))
}

residuals.lm <- function(x) x$residuals
fitted.lm <- function(x) x$fitted.values
coef.lm <- function(x) x$coefficients
weights.lm <- function(x) x$weights
df.residual.lm <- function(x) x$df.residual
deviance.lm <- function(x) sum((x$residuals)^2)
formula.lm <- function(x) formula(x$terms)
family.lm <- function(x) { gaussian() }

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
	if(full)dimnames(obj$qr$qr)[[2]]
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
	ssr <- if(is.null(w)) sum(resid(object)^2) else sum(w*resid(object)^2)
	p1 <- 1:object$rank
	comp <- object$effects[p1]
	asgn <- object$assign[object$qr$pivot][p1]
	dfr <- df.residual(object)
	ss <- c(as.numeric(lapply(split(comp^2,asgn),sum)),ssr)
	df <- c(as.numeric(lapply(split(asgn,  asgn),length)), dfr)
	if(attr(object$terms,"intercept")) {
		ss <- ss[-1]
		df <- df[-1]
	}
	ms <- ss/df
	f <- ms/(ssr/dfr)
	p <- 1 - pf(f,df,dfr)
	table <- cbind(df,ss,ms,f,p)
	table[length(p),4:5] <- NA
	dimnames(table) <- list(c(attr(object$terms,"term.labels"), "Residual"),
                                c("Df","Sum Sq", "Mean Sq", "F", "Pr(>F)"))
	result <- list(table=table,
		       title=paste("Analysis of Variance Table\nResponse:",
			 formula(object)[[2]]))
	class(result) <- "tabular"
	result
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
	# calculate the number of models
	nmodels <- length(objects)
	if (nmodels == 1)
		return(anova.lm(object))

	models <- as.character(lapply(objects, function(x) x$terms))

	# extract statistics
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
		else {
			f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
			p[i] <- 1 - pf(f[i], -df[i], df.r[i-1])
		}
	}
	table <- cbind(df.r,ss.r,df,ss,f,p)
	dimnames(table) <- list(1:nmodels, c("Res.Df", "Res.Sum-Sq", "Df",
		"Sum-Sq", "F", "Pr(>F)"))

	# construct table and title
	title <- "Analysis of Variance Table"
	topnote <- paste("Model ", format(1:nmodels),": ",
				models, sep="", collapse="\n")

	# calculate test statistic if needed
	output <- list(table = table, title = title, topnote=topnote)
	class(output) <- "tabular"
	return(output)
}

print.anova.lm <- function(x, digits = max(3, .Options$digits - 3), ...)
{
	cat("\nAnalysis of Variance:\n\n")
	print.default(round(unclass(x), digits), na="", print.gap=2)
	cat("\n")
	invisible(x)
}

predict.lm <- function (object, newdata = model.frame(object),
			conf.level=0.95, tol.level=conf.level)
{
	form <- delete.response(terms(object))
	X <- model.matrix(form,newdata)
	n <- NROW(object$qr$qr)
	p <- object$rank
	p1 <- 1:p
	piv <- object$qr$pivot[p1]
	r <- resid(object)
	f <- fitted(object)
	w <- weights(object)
	rss <- sum(if(is.null(w)) r^2 else w*r^2)
	R <- chol2inv(object$qr$qr[p1, p1, drop = FALSE])
	est <- object$coefficients[piv]
	predictor <- c(X[,piv,drop=F] %*% est)
	ip <- real(NROW(X))
	resvar <- rss/(n - p)
	vcov <- resvar * R
	for (i in (1:NROW(X))) {
		xi <- X[i,piv]
		ip[i] <- xi %*% vcov %*% xi
	}
	stderr1 <- sqrt(ip)
	stderr2 <- sqrt(resvar + ip)
	tt1 <- qt((1-conf.level)/2, n - p)
	tt2 <- qt((1- tol.level)/2, n - p)
	conf.l <- predictor + tt1 * stderr1
	conf.u <- predictor - tt1 * stderr1
	pred.l <- predictor + tt2 * stderr2
	pred.u <- predictor - tt2 * stderr2
	data.frame(predictor=predictor, conf.l=conf.l, conf.u=conf.u,
	pred.l=pred.l,pred.u=pred.u,row.names=rownames(newdata))
}


effects.lm <- function(...) .NotYetImplemented()

## Old version below, did it ever work?

## effects.lm <- function(z, term) {
##  term <- deparse(substitute(term))
##  k <- match(term,attr(z$terms,"term.labels"))
##  if(is.na(k)) stop("effect not found")
##  pattern <- attr(z$terms,"factors")[,k]
##  factors <- as.logical(lapply(z$model.frame,is.factor))
##  y <- model.response(z$model.frame,"numeric")
##  k <- range(seq(length(z$assign))[z$assign==k])
##  yhat0 <- if(k[1] > 1) qr.fitted(z$qr,y,k[1]-1) else 0
##  yhat1 <- qr.fitted(z$qr,y,k[2])
##  effects <- yhat1-yhat0
##  tapply(effects,z$model.frame[factors & pattern!=0],mean,na.rm=TRUE)
##}

plot.lm <- function(...) .NotYetImplemented()
