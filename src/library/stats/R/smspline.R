smooth.spline <-
  function(x, y = NULL, w = NULL, df, spar = NULL, cv = FALSE,
	   all.knots = FALSE, nknots = NULL,
           df.offset = 0, penalty = 1, control.spar = list())
{
    sknotl <- function(x, nk = NULL)
    {
        ## if (!all.knots)
	## return reasonable sized knot sequence for INcreasing x[]:
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
        n <- length(x)
        if(is.null(nk)) nk <- n.kn(n)
        else if(!is.numeric(nk)) stop("'nknots' must be numeric <= n")
        else if(nk > n)
            stop("cannot use more inner knots than unique 'x' values")
	c(rep(x[1], 3), x[seq(1,n, len= nk)], rep(x[n], 3))
    }
    contr.sp <- list(low = -1.5,## low = 0.      was default till R 1.3.x
                     high = 1.5,
                     tol = 1e-4,## tol = 0.001   was default till R 1.3.x
                     eps = 2e-8,## eps = 0.00244 was default till R 1.3.x
                     maxit = 500, trace = getOption("verbose"))
    contr.sp[(names(control.spar))] <- control.spar
    if(!all(sapply(contr.sp[1:4],is.double)) ||
       contr.sp$tol < 0 || contr.sp$eps <= 0 || contr.sp$maxit <= 0)
        stop("invalid 'control.spar'")

    xy <- xy.coords(x, y)
    y <- xy$y
    x <- xy$x
    n <- length(x)
    w <-
	if(is.null(w)) rep(1, n)
	else {
	    if(n != length(w)) stop("lengths of 'x' and 'w' must match")
	    if(any(w < 0)) stop("all weights should be non-negative")
	    if(all(w == 0)) stop("some weights should be positive")
	    (w * sum(w > 0))/sum(w)
	}# now sum(w) == #{obs. with weight > 0} == sum(w > 0)

    ## Replace y[] for same x[] (to 6 digits precision) by their mean :
    x <- signif(x, 6)
    ux <- unique(sort(x))
    ox <- match(x, ux)
    tmp <- matrix(unlist(tapply(seq(along=y), ox,
				function(i,y,w)
				c(sum(w[i]), sum(w[i]*y[i]),sum(w[i]*y[i]^2)),
				y = y, w = w)),
		  ncol = 3, byrow=TRUE)
    wbar <- tmp[, 1]
    ybar <- tmp[, 2]/ifelse(wbar > 0, wbar, 1)
    yssw <- sum(tmp[, 3] - wbar*ybar^2) # will be added to RSS for GCV
    nx <- length(ux)
    if(nx <= 3) stop("need at least four unique 'x' values")
    if(cv && nx < n)
        warning("crossvalidation with non-unique 'x' values seems doubtful")
    r.ux <- ux[nx] - ux[1]
    xbar <- (ux - ux[1])/r.ux           # scaled to [0,1]
    if(all.knots) {
	knot <- c(rep(xbar[1], 3), xbar, rep(xbar[nx], 3))
	nk <- nx + 2
    } else {
	knot <- sknotl(xbar, nknots)
	nk <- length(knot) - 4
    }

    ## ispar != 1 : compute spar (later)
    ispar <-
        if(is.null(spar) || missing(spar)) { ## || spar == 0
            if(contr.sp$trace) -1 else 0
        } else 1
    spar <- if(ispar == 1) as.double(spar) else double(1)
    ## was <- if(missing(spar)) 0 else if(spar < 1.01e-15) 0 else  1
    ## icrit {../src/sslvrg.f}:
    ##		(0 = no crit,  1 = GCV ,  2 = ord.CV , 3 = df-matching)
    icrit <- if(cv) 2 else  1
    dofoff <- df.offset
    if(!missing(df)) {
	if(df > 1 && df <= nx) {
	    icrit <- 3
	    dofoff <- df
	} else warning("you must supply 1 < df <= n,  n = #{unique x} = ", nx)
    }
    iparms <- as.integer(c(icrit,ispar, contr.sp$maxit))
    names(iparms) <- c("icrit", "ispar", "iter")

    fit <- .Fortran("qsbart",		# code in ../src/qsbart.f
		    as.double(penalty),
		    as.double(dofoff),
		    x = as.double(xbar),
		    y = as.double(ybar),
		    w = as.double(wbar),
		    ssw = as.double(yssw),
		    as.integer(nx),
		    as.double(knot),
		    as.integer(nk),
		    coef = double(nk),
		    ty = double(nx),
		    lev = double(nx),
		    crit = double(1),
		    iparms = iparms,
		    spar = spar,
		    parms = unlist(contr.sp[1:4]),
		    isetup = as.integer(0),
		    scrtch = double((17 + 0) * nk + 1),
		    ld4  = as.integer(4),
		    ldnk = as.integer(1),
		    ier = integer(1),
		    DUP = FALSE, PACKAGE="stats"
		    )[c("coef","ty","lev","spar","parms","crit","iparms","ier")]

    lev <- fit$lev
    df <- sum(lev)
    if(is.na(df))
	stop("NA lev[]; probably smoothing parameter 'spar' way too large!")
    if(fit$ier > 0 ) {
        sml <- fit$spar < 0.5
	wtxt <- paste("smoothing parameter value too",
                      if(sml) "small" else "large")
        if(sml) {
            ## used to give warning too and mean() as below, but that's rubbish
            stop(wtxt)
        } else {
            fit$ty <- rep(mean(y), nx) ## would be df = 1
            df <- 1
            warning(wtxt,"\nsetting df = 1  __use with care!__")
        }
    }
    cv.crit <-
	if(cv) {
	    ww <- wbar
	    ww[!(ww > 0)] <- 1
	    weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
	} else weighted.mean((y - fit$ty[ox])^2, w)/
	    (1 - (df.offset + penalty * df)/n)^2
    pen.crit <- sum(wbar * (ybar - fit$ty)^2)
    fit.object <- list(knot = knot, nk = nk, min = ux[1], range = r.ux,
		       coef = fit$coef)
    class(fit.object) <- "smooth.spline.fit"
    ## parms :  c(low = , high = , tol = , eps = )
    object <- list(x = ux, y = fit$ty, w = wbar, yin = ybar,
		   lev = lev, cv.crit = cv.crit, pen.crit = pen.crit,
                   crit = fit$crit,
                   df = df, spar = fit$spar,
                   lambda = unname(fit$parms["low"]),
                   iparms = fit$iparms, # c(icrit= , ispar= , iter= )
                   fit = fit.object, call = match.call())
    class(object) <- "smooth.spline"
    object
}

print.smooth.spline <- function(x, digits = getOption("digits"), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl)
    }
    ip <- x$iparms
    cv <- cl$cv
    if(is.null(cv)) cv <- FALSE else if(is.name(cv)) cv <- eval(cv)
    cat("\nSmoothing Parameter  spar=", format(x$spar, digits=digits),
        " lambda=",format(x$lambda, digits=digits),
        if(ip["ispar"] != 1) paste("(",ip["iter"]," iterations)", sep=""),
        "\n")
    cat("Equivalent Degrees of Freedom (Df):", format(x$df,digits=digits),"\n")
    cat("Penalized Criterion:", format(x$pen.crit, digits=digits), "\n")
    cat(if(cv) "PRESS:" else "GCV:", format(x$cv.crit, digits=digits), "\n")
    invisible(x)
}

predict.smooth.spline <- function(object, x, deriv = 0, ...)
{
    if(missing(x)) {
        if(deriv == 0)
            return(object[c("x", "y")])
        else x <- object$x
    }
    fit <- object$fit
    if(is.null(fit)) stop("not a valid \"smooth.spline\" object")
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
			      DUP = FALSE, PACKAGE="stats")$s
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
    if(!n || !is.numeric(y)) stop("'y' must be numeric vector")
    if(length(x) != n) stop("number of observations in 'x' and 'y' must match.")
    if(length(wt) != n)
	stop("number of weights must match number of observations.")
    if(span < 0 || span > 1) stop("'span' must be between 0 and 1.")
    if(periodic) {
	iper <- 2
	xrange <- range(x)
	if(xrange[1] < 0 || xrange[2] > 1)
	    stop("'x' must be between 0 and 1 for periodic smooth")
    } else iper <- 1
    okay <- is.finite(x + y + wt)
    ord <- order(x[okay], y[okay])
    ord <- cumsum(!okay)[okay][ord] + ord
    xo <- x[ord]
    leno <- length(ord)
    if(diff <- n - leno)
	warning(diff, " observation(s) with NAs, NaNs and/or Infs deleted")
    .Fortran("setsmu", PACKAGE = "stats")
    smo <- .Fortran("supsmu",
		    as.integer(leno),
		    as.double(xo),
		    as.double(y[ord]),
		    as.double(wt[ord]),
		    as.integer(iper),
		    as.double(span),
		    as.double(bass),
		    smo=double(leno),
		    double(n*7), double(1),
		    PACKAGE = "stats")$smo
    ## eliminate duplicate xsort values and corresponding smoothed values
    dupx <- duplicated(xo)
    list(x = xo[!dupx], y = smo[!dupx])
}
