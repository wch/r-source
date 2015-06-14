#  File src/library/stats/R/ppr.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 2000-2015 The R Core Team
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
#  http://www.r-project.org/Licenses/

ppr <- function(x, ...) UseMethod("ppr")

ppr.formula <-
function(formula, data, weights, subset,
	 na.action, contrasts = NULL, ..., model = FALSE)
{
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$contrasts <- m$... <- NULL
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0L
    X <- model.matrix(Terms, m, contrasts)
    Y <- model.response(m)
    w <- model.weights(m)
    if(length(w) == 0L) w <- rep_len(1, nrow(X))
    fit <- ppr.default(X, Y, w, ...)
    fit$na.action <- attr(m, "na.action")
    fit$terms <- Terms
    ## fix up call to refer to the generic, but leave arg name as `formula'
    call[[1L]] <- as.name("ppr")
    fit$call <- call
    fit$contrasts <- attr(X, "contrasts")
    fit$xlevels <- .getXlevels(Terms, m)
    if(model) fit$model <- m
    structure(fit, class=c("ppr.form", "ppr"))
}

ppr.default <-
function(x, y, weights=rep(1,n), ww=rep(1,q), nterms, max.terms=nterms,
	 optlevel=2, sm.method=c("supsmu", "spline", "gcvspline"),
	 bass=0, span=0, df=5, gcvpen=1, ...)
{
    call <- match.call()
    call[[1L]] <- as.name("ppr")
    sm.method <- match.arg(sm.method)
    ism <- switch(sm.method, supsmu=0, spline=1, gcvspline=2)
    if(missing(nterms)) stop("'nterms' is missing with no default")
    mu <- nterms; ml <- max.terms
    x <- as.matrix(x)
    y <- as.matrix(y)
    if(!is.numeric(x) || !is.numeric(y))
        stop("'ppr' applies only to numerical variables")
    n <- nrow(x)
    if(nrow(y) != n) stop("mismatched 'x' and 'y'")
    p <- ncol(x)
    q <- ncol(y)
    if(!is.null(dimnames(x))) xnames <- dimnames(x)[[2L]]
    else xnames <- paste0("X", 1L:p)
    if(!is.null(dimnames(y))) ynames <- dimnames(y)[[2L]]
    else ynames <- paste0("Y", 1L:q)
    msmod <- ml*(p+q+2*n)+q+7+ml+1	# for asr
    nsp <- n*(q+15)+q+3*p
    ndp <- p*(p+1)/2+6*p
    .Fortran(C_setppr,
	     as.double(span), as.double(bass), as.integer(optlevel),
	     as.integer(ism), as.double(df), as.double(gcvpen)
	     )
    Z <- .Fortran(C_smart,
		  as.integer(ml), as.integer(mu),
		  as.integer(p), as.integer(q), as.integer(n),
		  as.double(weights),
		  as.double(t(x)),
		  as.double(t(y)),
		  as.double(ww),
		  smod=double(msmod), as.integer(msmod),
		  double(nsp), as.integer(nsp),
		  double(ndp), as.integer(ndp),
		  edf=double(ml)
		  )
    smod <- Z$smod
    ys <- smod[q+6]
    tnames <- paste("term", 1L:mu)
    alpha <- matrix(smod[q+6L + 1L:(p*mu)],p, mu,
		    dimnames=list(xnames, tnames))
    beta <- matrix(smod[q+6L+p*ml + 1L:(q*mu)], q, mu,
		   dimnames=list(ynames, tnames))
    fitted <- drop(matrix(.Fortran(C_pppred,
				   as.integer(nrow(x)),
				   as.double(x),
				   as.double(smod),
				   y = double(nrow(x)*q),
				   double(2*smod[4L]))$y,
			  ncol=q, dimnames=dimnames(y)))
    jt <- q + 7 + ml*(p+q+2*n)
    gof <- smod[jt] * n * ys^2
    gofn <- smod[jt+1L:ml] * n * ys^2
    ## retain only terms for the size of model finally fitted
    jf <- q+6+ml*(p+q)
    smod <- smod[c(1L:(q+6+p*mu), q+6+p*ml + 1L:(q*mu),
		   jf + 1L:(mu*n), jf+ml*n + 1L:(mu*n))]
    smod[1L] <- mu
    structure(list(call=call, mu=mu, ml=ml, p=p, q=q,
		   gof=gof, gofn=gofn,
		   df=df, edf=Z$edf[1L:mu],
		   xnames=xnames, ynames=ynames,
		   alpha=drop(alpha), beta=ys*drop(beta),
		   yb=smod[5+1L:q], ys=ys,
		   fitted.values=fitted, residuals=drop(y-fitted),
		   smod=smod),
	      class="ppr")
}

print.ppr <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    mu <- x$mu; ml <- x$ml
    cat("\nGoodness of fit:\n")
    gof <- setNames(x$gofn, paste(1L:ml, "terms"))
    print(format(gof[mu:ml], ...), quote=FALSE)
    invisible(x)
}

summary.ppr <- function(object, ...)
{
    class(object) <- "summary.ppr"
    object
}

print.summary.ppr <- function(x, ...)
{
    print.ppr(x, ...)
    mu <- x$mu
    cat("\nProjection direction vectors:\n")
    print(format(x$alpha, ...), quote=FALSE)
    cat("\nCoefficients of ridge terms:\n")
    print(format(x$beta, ...), quote=FALSE)
    if(any(x$edf >0)) {
	cat("\nEquivalent df for ridge terms:\n")
	edf <- setNames(x$edf, paste("term", 1L:mu))
	print(round(edf,2), ...)
    }
    invisible(x)
}

plot.ppr <- function(x, ask, type="o", ...)
{
    ppr.funs <- function(obj)
    {
	## cols for each term
	p <- obj$p; q <- obj$q
	sm <- obj$smod
	n <- sm[4L]; mu <- sm[5L]; m <- sm[1L]
	jf <- q+6+m*(p+q)
	jt <- jf+m*n
	f <- matrix(sm[jf+1L:(mu*n)],n, mu)
	t <- matrix(sm[jt+1L:(mu*n)],n, mu)
	list(x=t, y=f)
    }
    obj <- ppr.funs(x)
    if(!missing(ask)) {
        oask <- devAskNewPage(ask)
        on.exit(devAskNewPage(oask))
    }
    for(i in 1L:x$mu) {
	ord <- order(obj$x[ ,i])
	plot(obj$x[ord, i], obj$y[ord, i], type = type,
	     xlab = paste("term", i), ylab = "", ...)
    }
    invisible()
}

predict.ppr <- function(object, newdata, ...)
{
    if(missing(newdata)) return(fitted(object))
    if(!is.null(object$terms)) {
        newdata <- as.data.frame(newdata)
        rn <- row.names(newdata)
# work hard to predict NA for rows with missing data
        Terms <- delete.response(object$terms)
        m <- model.frame(Terms, newdata, na.action = na.omit,
                         xlev = object$xlevels)
        if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
        keep <- match(row.names(m), rn)
        x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    } else {
        x <- as.matrix(newdata)
        keep <- seq_len(nrow(x))
        rn <- dimnames(x)[[1L]]
    }
    if(ncol(x) != object$p) stop("wrong number of columns in 'x'")
    res <- matrix(NA, length(keep), object$q,
                  dimnames = list(rn, object$ynames))
    res[keep, ] <- matrix(.Fortran(C_pppred,
                                   as.integer(nrow(x)),
                                   as.double(x),
                                   as.double(object$smod),
                                   y = double(nrow(x)*object$q),
                                   double(2*object$smod[4L])
                                   )$y, ncol=object$q)
    drop(res)
}
