# file modreg/R/ppr.R
# copyright (C) 1998 B. D. Ripley
# Copyright (C) 2000 The R Development Core Team
#
ppr <- function(x, ...) UseMethod("ppr")

ppr.formula <-
function(formula, data=sys.parent(), weights, subset,
	 na.action, contrasts=NULL, ...)
{
    call <- match.call()
    m <- match.call(expand = FALSE)
    m$contrasts <- m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    X <- model.matrix(Terms, m, contrasts)
    Y <- model.extract(m, response)
    w <- model.extract(m, weights)
    if(length(w) == 0) w <- rep(1, nrow(X))
    fit <- ppr.default(X, Y, w, ...)
    fit$terms <- Terms
    fit$call <- call
    structure(fit, class=c("ppr.form", "ppr"))
}

ppr.default <-
function(x, y, weights=rep(1,n), ww=rep(1,q), nterms, max.terms=nterms,
	 optlevel=2, sm.method=c("supsmu", "spline", "gcvspline"),
	 bass=0, span=0, df=5, gcvpen=1)
{
    call <- match.call()
    sm.method <- match.arg(sm.method)
    ism <- switch(sm.method, supsmu=0, spline=1, gcvspline=2)
    if(missing(nterms)) stop("nterms is missing with no default")
    mu <- nterms; ml <- max.terms
    x <- as.matrix(x)
    y <- as.matrix(y)
    n <- nrow(x)
    if(nrow(y) != n) stop("mismatched x and y")
    p <- ncol(x)
    q <- ncol(y)
    if(!is.null(dimnames(x))) xnames <- dimnames(x)[[2]]
    else xnames <- paste("X", 1:p, sep="")
    if(!is.null(dimnames(y))) ynames <- dimnames(y)[[2]]
    else ynames <- paste("Y", 1:q, sep="")
    msmod <- ml*(p+q+2*n)+q+7+ml+1	# for asr
    nsp <- n*(q+15)+q+3*p
    ndp <- p*(p+1)/2+6*p
    .Fortran("bdrsetppr",
	     as.double(span), as.double(bass), as.integer(optlevel),
	     as.integer(ism), as.double(df), as.double(gcvpen),
	     PACKAGE="modreg"
	     )
    Z <- .Fortran("bdrsmart",
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
		  PACKAGE="modreg"
		  )
    smod <- Z$smod
    ys <- smod[q+6]
    tnames <- paste("term", 1:mu)
    alpha <- matrix(smod[q+6 + 1:(p*mu)],p, mu,
		    dimnames=list(xnames, tnames))
    beta <- matrix(smod[q+6+p*ml + 1:(q*mu)], q, mu,
		   dimnames=list(ynames, tnames))
    fitted <- drop(matrix(.Fortran("bdrpred",
				   as.integer(nrow(x)),
				   as.double(x),
				   as.double(smod),
				   y = double(nrow(x)*q),
				   double(2*smod[4]),
				   PACKAGE="modreg")$y,
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

plot.ppr <- function(fit, ask, type="o", ...)
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
    obj <- ppr.funs(fit)
    if(!missing(ask)) {
	oldpar <- par()
	on.exit(par(oldpar))
	par(ask = ask)
    }
    for(i in 1:fit$mu) {
	ord <- order(obj$x[ ,i])
	plot(obj$x[ord, i], obj$y[ord, i], type = type,
	     xlab = paste("term", i), ylab = "", ...)
    }
}

predict.ppr <- function(obj, newdata, ...)
{
    if(missing(newdata)) return(obj$fitted)
    if(!is.null(obj$terms))
	x <- model.matrix(delete.response(obj$terms), newdata)
    else x <- as.matrix(newdata)
    if(ncol(x) != obj$p) stop("wrong number of columns in x")
    drop(matrix(.Fortran("bdrpred",
			 as.integer(nrow(x)),
			 as.double(x),
			 as.double(obj$smod),
			 y = double(nrow(x)*obj$q),
			 double(2*obj$smod[4]),
			 PACKAGE="modreg"
			 )$y,
		ncol=obj$q,
		dimnames=list(dimnames(x)[[1]], obj$ynames)
		))
}
