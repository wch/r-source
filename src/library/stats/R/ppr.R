# file stats/R/ppr.R
# copyright (C) 1998 B. D. Ripley
# Copyright (C) 2000-3 The R Development Core Team
#
ppr <- function(x, ...) UseMethod("ppr")

ppr.formula <-
function(formula, data, weights, subset,
	 na.action, contrasts = NULL, ..., model = FALSE)
{
    call <- match.call()
    m <- match.call(expand = FALSE)
    m$contrasts <- m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    X <- model.matrix(Terms, m, contrasts)
    Y <- model.response(m)
    w <- model.weights(m)
    if(length(w) == 0) w <- rep(1, nrow(X))
    fit <- ppr.default(X, Y, w, ...)
    fit$na.action <- attr(m, "na.action")
    fit$terms <- Terms
    ## fix up call to refer to the generic, but leave arg name as `formula'
    call[[1]] <- as.name("ppr")
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
    call[[1]] <- as.name("ppr")
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
    if(!is.null(dimnames(x))) xnames <- dimnames(x)[[2]]
    else xnames <- paste("X", 1:p, sep="")
    if(!is.null(dimnames(y))) ynames <- dimnames(y)[[2]]
    else ynames <- paste("Y", 1:q, sep="")
    msmod <- ml*(p+q+2*n)+q+7+ml+1	# for asr
    nsp <- n*(q+15)+q+3*p
    ndp <- p*(p+1)/2+6*p
    .Fortran("setppr",
	     as.double(span), as.double(bass), as.integer(optlevel),
	     as.integer(ism), as.double(df), as.double(gcvpen),
	     PACKAGE="stats"
	     )
    Z <- .Fortran("smart",
		  as.integer(ml), as.integer(mu),
		  as.integer(p), as.integer(q), as.integer(n),
		  as.double(weights),
		  as.double(t(x)),
		  as.double(t(y)),
		  as.double(ww),
		  smod=double(msmod), as.integer(msmod),
		  double(nsp), as.integer(nsp),
		  double(ndp), as.integer(ndp),
		  edf=double(ml),
		  PACKAGE="stats"
		  )
    smod <- Z$smod
    ys <- smod[q+6]
    tnames <- paste("term", 1:mu)
    alpha <- matrix(smod[q+6 + 1:(p*mu)],p, mu,
		    dimnames=list(xnames, tnames))
    beta <- matrix(smod[q+6+p*ml + 1:(q*mu)], q, mu,
		   dimnames=list(ynames, tnames))
    fitted <- drop(matrix(.Fortran("pppred",
				   as.integer(nrow(x)),
				   as.double(x),
				   as.double(smod),
				   y = double(nrow(x)*q),
				   double(2*smod[4]),
				   PACKAGE="stats")$y,
			  ncol=q, dimnames=dimnames(y)))
    jt <- q + 7 + ml*(p+q+2*n)
    gof <- smod[jt] * n * ys^2
    gofn <- smod[jt+1:ml] * n * ys^2
    ## retain only terms for the size of model finally fitted
    jf <- q+6+ml*(p+q)
    smod <- smod[c(1:(q+6+p*mu), q+6+p*ml + 1:(q*mu),
		   jf + 1:(mu*n), jf+ml*n + 1:(mu*n))]
    smod[1] <- mu
    structure(list(call=call, mu=mu, ml=ml, p=p, q=q,
		   gof=gof, gofn=gofn,
		   df=df, edf=Z$edf[1:mu],
		   xnames=xnames, ynames=ynames,
		   alpha=drop(alpha), beta=ys*drop(beta),
		   yb=smod[5+1:q], ys=ys,
		   fitted.values=fitted, residuals=drop(y-fitted),
		   smod=smod),
	      class="ppr")
}

print.ppr <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl)
    }
    mu <- x$mu; ml <- x$ml
    cat("\nGoodness of fit:\n")
    gof <- x$gofn; names(gof) <- paste(1:ml, "terms")
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
	edf <- x$edf; names(edf) <- paste("term", 1:mu)
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
	n <- sm[4]; mu <- sm[5]; m <- sm[1]
	jf <- q+6+m*(p+q)
	jt <- jf+m*n
	f <- matrix(sm[jf+1:(mu*n)],n, mu)
	t <- matrix(sm[jt+1:(mu*n)],n, mu)
	list(x=t, y=f)
    }
    obj <- ppr.funs(x)
    if(!missing(ask)) {
	oldpar <- par()
	on.exit(par(oldpar))
	par(ask = ask)
    }
    for(i in 1:x$mu) {
	ord <- order(obj$x[ ,i])
	plot(obj$x[ord, i], obj$y[ord, i], type = type,
	     xlab = paste("term", i), ylab = "", ...)
    }
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
        x <- model.matrix(Terms, m, contrasts = object$contrasts)
    } else {
        x <- as.matrix(newdata)
        keep <- 1:nrow(x)
        rn <- dimnames(x)[[1]]
    }
    if(ncol(x) != object$p) stop("wrong number of columns in 'x'")
    res <- matrix(NA, length(keep), object$q,
                  dimnames = list(rn, object$ynames))
    res[keep, ] <- matrix(.Fortran("pppred",
                                   as.integer(nrow(x)),
                                   as.double(x),
                                   as.double(object$smod),
                                   y = double(nrow(x)*object$q),
                                   double(2*object$smod[4]),
                                   PACKAGE="stats"
                                   )$y, ncol=object$q)
    drop(res)
}
