#### copyright (C) 1998 B. D. Ripley
#### Copyright (C) 2000 The R Development Core Team

smooth.spline <-
  function(x, y = NULL, w = NULL, df = 5, spar = 0, cv = FALSE,
	   all.knots = FALSE, df.offset = 0, penalty = 1)
{
    sknotl <- function(x)
    {
	## Return reasonable sized knot sequence for INcreasing x[]:
	n.kn <- function(n) {
	    ## Number of inner knots
	    if(n < 50) n
	    else trunc({
		a1 <- log( 50, 2)
		a2 <- log(100, 2)
		a3 <- log(140, 2)
		a4 <- log(200, 2)
		if	(n < 200) 2^(a1+(a2-a1)*(n-50)/150)
		else if (n < 800) 2^(a2+(a3-a2)*(n-200)/600)
		else if (n < 3200)2^(a3+(a4-a3)*(n-800)/2400)
		else  200 + (n-3200)^0.2
	    })
	}
	nk <- n.kn( n <- length(x) )
	c(rep(x[1], 3), x[seq(1,n, len= nk)], rep(x[n], 3))
    }

    xy <- xy.coords(x, y)
    y <- xy$y
    x <- xy$x
    n <- length(x)
    w <-
	if(is.null(w)) rep(1, n)
	else {
	    if(n != length(w)) stop("lengths of x and w must match")
	    if(any(w < 0)) stop("all weights should be non-negative")
	    (w * sum(w > 0))/sum(w)
	}
    ispar <- if(missing(spar)) 0 else if(spar < 1.01e-15) 0 else  1
    icrit <- if(cv) 2 else  1
    dfinfo <- df.offset
    if(!missing(df)) {
	if(df > 1 & df < n) {
	    icrit <- 3
	    dfinfo <- df
	} else warning("you must supply 1 < df < n")
    }
    x <- signif(x, 6)
    ux <- unique(sort(x))
    ox <- match(x, ux)
    tmp <- matrix(unlist(tapply(seq(along=y), ox,
				function(i,y,w) c(mean(y[i]), sum(w[i])),
				y = y, w = w)),
		  ncol = 2, byrow=TRUE)
    ybar <- tmp[, 1]
    wbar <- tmp[, 2]
    nx <- length(ux)
    r.ux <- ux[nx] - ux[1]
    xbar <- (ux - ux[1])/r.ux
    if(all.knots) {
	knot <- c(rep(xbar[1], 3), xbar, rep(xbar[nx], 3))
	nk <- nx + 2
    } else {
	knot <- sknotl(xbar)
	nk <- length(knot) - 4
    }
    low.parm <- 0
    high.parm <- 1.5
    fit <- .Fortran("qsbart",
		    as.double(penalty),
		    as.double(dfinfo),
		    x = as.double(xbar),
		    y = as.double(ybar),
		    w = as.double(wbar),
		    as.integer(nx),
		    as.double(knot),
		    as.integer(nk),
		    coef = double(nk),
		    ty = double(nx),
		    lev = double(nx),
		    crit = double(1),
		    iparms =as.integer(c(icrit, ispar)),
		    spar = as.double(spar),
		    parms= as.double(c(0, 1.5, 0.001)),
		    isetup= as.integer(0),
		    scrtch= double((17 + nk) * nk),
		    ld4= as.integer(4),
		    ldnk= as.integer(1),
		    ier = as.integer(1),
		    DUP = FALSE, PACKAGE="modreg"
		    )[c("coef","ty","lev","spar","ier")]
    if(fit$ier > 0) {
	warning("smoothing parameter value too small or too large")
	fit$ty <- rep(mean(y), nx)
    }
    lev <- fit$lev
    df <- sum(lev)
    cv.crit <-
	if(cv) {
	    ww <- wbar
	    ww[!(ww > 0)] <- 1
	    weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
	} else weighted.mean((y - fit$ty[ox])^2, w)/
	    (1 - (df.offset + penalty * df)/sum(wbar))^2
    pen.crit <- sum(wbar * (ybar - fit$ty) * ybar)
    fit.object <- list(knot = knot, nk = nk, min = ux[1], range = r.ux,
		       coef = fit$coef)
    class(fit.object) <- "smooth.spline.fit"
    object <- list(x = ux, y = fit$ty, w = wbar, yin = ybar,
		   lev = lev, cv.crit = cv.crit, pen.crit = pen.crit, df = df,
		   spar = fit$spar, fit = fit.object, call = match.call())
    class(object) <- "smooth.spline"
    object
}

print.smooth.spline <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl)
    }
    if(is.null(cv <- cl$cv)) cv <- FALSE
    cat("\nSmoothing Parameter (Spar):", format(x$spar), "\n")
    cat("Equivalent Degrees of Freedom (Df):", format(x$df), "\n")
    cat("Penalized Criterion:", format(x$pen.crit), "\n")
    crss <- if(cv) "PRESS:" else "GCV:"
    cat(crss, format(x$cv.crit), "\n")
    invisible(x)
}

predict.smooth.spline <- function(object, x, deriv = 0, ...)
{
    if(missing(x)) return(object[c("x", "y")])
    fit <- object$fit
    if(is.null(fit)) stop("not a valid smooth.spline object")
    else predict(fit, x, deriv, ...)
}

predict.smooth.spline.fit <- function(object, x, deriv = 0, ...)
{
    if(missing(x))
	x <- seq(from = object$min, to = object$min + object$range,
		 length = length(object$coef) - 4)
    xs <- (x - object$min)/object$range # x scaled to [0,1]
    extrap.left <- xs < 0
    extrap.right <- xs > 1
    interp <- !(extrap <- extrap.left | extrap.right)
    n <- sum(interp) # number of xs in [0,1]
    y <- xs
    if(any(interp))
	y[interp] <- .Fortran("bvalus",
			      n	  = as.integer(n),
			      knot= as.double(object$knot),
			      coef= as.double(object$coef),
			      nk  = as.integer(object$nk),
			      x	  = as.double(xs[interp]),
			      s	  = double(n),
			      order= as.integer(deriv),
			      DUP = FALSE, PACKAGE="modreg")$s
    if(any(extrap)) {
	xrange <- c(object$min, object$min + object$range)
	if(deriv == 0) {
	    end.object <- Recall(object, xrange)$y
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    if(any(extrap.left))
		y[extrap.left] <- end.object[1] +
		    end.slopes[1] * (xs[extrap.left] - 0)
	    if(any(extrap.right))
		y[extrap.right] <- end.object[2] +
		    end.slopes[2] * (xs[extrap.right] - 1)
	} else if(deriv == 1) {
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    y[extrap.left] <- end.slopes[1]
	    y[extrap.right] <- end.slopes[2]
	}
	else y[extrap] <- 0
    }
    if(deriv > 0)
	y <- y/(object$range^deriv)
    list(x = x, y = y)
}

supsmu <-
  function(x, y, wt = rep(1, n), span = "cv", periodic = FALSE, bass = 0)
{
    if(span == "cv") span <- 0
    n <- length(y)
    if(!n || !is.numeric(y)) stop("`y' must be numeric vector")
    if(length(x) != n) stop("number of observations in x and y must match.")
    if(length(wt) != n)
	stop("number of weights must match number of observations.")
    if(span < 0 || span > 1) stop("span must be between 0 and 1.")
    if(periodic) {
	iper <- 2
	xrange <- range(x)
	if(xrange[1] < 0 || xrange[2] > 1)
	    stop("x must be between 0 and 1 for periodic smooth")
    } else iper <- 1
    okay <- is.finite(x + y + wt)
    ord <- order(x[okay], y[okay])
    ord <- cumsum(!okay)[okay][ord] + ord
    xo <- x[ord]
    leno <- length(ord)
    if(diff <- n - leno)
	warning(paste(diff, "observation(s) with NAs, NaNs and/or Infs deleted"))
    .Fortran("bdrsetsmu")
    smo <- .Fortran("bdrsupsmu",
		    as.integer(leno),
		    as.double(xo),
		    as.double(y[ord]),
		    as.double(wt[ord]),
		    as.integer(iper),
		    as.double(span),
		    as.double(bass),
		    smo=double(leno),
		    double(n*7), double(1),
		    PACKAGE="modreg")$smo
    ## eliminate duplicate xsort values and corresponding smoothed values
    dupx <- duplicated(xo)
    list(x = xo[!dupx], y = smo[!dupx])
}




