#  File src/library/stats/R/smspline.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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
#  https://www.R-project.org/Licenses/

.nknots.smspl <- function(n) {
    ## Number of inner knots
    if(n < 50L) n
    else trunc({
        a1 <- log2( 50)
        a2 <- log2(100)
        a3 <- log2(140)
        a4 <- log2(200)
        if	(n < 200L) 2^(a1+(a2-a1)*(n-50)/150)
        else if (n < 800L) 2^(a2+(a3-a2)*(n-200)/600)
        else if (n < 3200L)2^(a3+(a4-a3)*(n-800)/2400)
        else  200 + (n-3200)^0.2
    })
}
n.knots <- function(n) {
    message(".nknots.smspl() is now exported; use it instead of n.knots()")
    .nknots.smspl(n)
}

smooth.spline <-
    function(x, y = NULL, w = NULL, df, spar = NULL, cv = FALSE,
             all.knots = FALSE, nknots = .nknots.smspl, keep.data = TRUE,
             df.offset = 0, penalty = 1, control.spar = list(),
             tol = 1e-6 * IQR(x))
{
    contr.sp <- list(low = -1.5, # low = 0.      was default till R 1.3.x
                     high = 1.5,
                     tol = 1e-4, # tol = 0.001   was default till R 1.3.x
                     eps = 2e-8, # eps = 0.00244 was default till R 1.3.x
                     maxit = 500, trace = getOption("verbose"))
    contr.sp[names(control.spar)] <- control.spar
    if(!all(sapply(contr.sp[1:4], is.numeric)) ||
       contr.sp$tol < 0 || contr.sp$eps <= 0 || contr.sp$maxit <= 0)
        stop("invalid 'control.spar'")

    xy <- xy.coords(x, y)
    y <- xy$y
    x <- xy$x
    if(!all(is.finite(c(x, y))))
        stop("missing or infinite values in inputs are not allowed")
    n <- length(x)
    if(is.na(n)) stop("invalid number of points")
    w <-
	if(is.null(w)) rep_len(1, n)
	else {
	    if(n != length(w)) stop("lengths of 'x' and 'w' must match")
	    if(any(w < 0)) stop("all weights should be non-negative")
	    if(all(w == 0)) stop("some weights should be positive")
	    (w * sum(w > 0))/sum(w)
	} # now sum(w) == #{obs. with weight > 0} == sum(w > 0)

    ## Replace y[], w[] for same x[] (to a precision of 'tol') by their mean :
    if(!is.finite(tol) || tol <= 0)
        stop("'tol' must be strictly positive and finite")
    xx <- round((x - mean(x))/tol)  # de-mean to avoid possible overflow
    nd <- !duplicated(xx); ux <- sort(x[nd]); uxx <- sort(xx[nd])
    nx <- length(ux)
    if(nx <= 3L) stop("need at least four unique 'x' values")
    if(nx == n) { # speedup
	ox <- TRUE
	tmp <- cbind(w, w*y, w*y^2)[order(x),]
    } else {
	ox <- match(xx, uxx)
	## Faster, much simplified version of tapply()
	tapply1 <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
	    sapply(X = unname(split(X, INDEX)), FUN = FUN, ...,
		   simplify = simplify, USE.NAMES = FALSE)
	}
	tmp <- matrix(unlist(tapply1(seq_len(n), ox,
				     function(i, y, w)
				     c(sum(w[i]), sum(w[i]*y[i]),sum(w[i]*y[i]^2)),
				     y = y, w = w), use.names = FALSE),
		      ncol = 3, byrow = TRUE)
    }
    wbar <- tmp[, 1L]
    ybar <- tmp[, 2L]/ifelse(wbar > 0, wbar, 1)
    yssw <- sum(tmp[, 3L] - wbar*ybar^2) # will be added to RSS for GCV
    ## Note: now  cv in {NA,FALSE,TRUE}
    if(is.na(cv) && !missing(df))
	stop("'cv' must not be NA when 'df' is specified")
    CV <- !is.na(cv) && cv
    if(CV && nx < n)
        warning("cross-validation with non-unique 'x' values seems doubtful")
    r.ux <- ux[nx] - ux[1L]
    xbar <- (ux - ux[1L])/r.ux           # scaled to [0,1]
    if(all.knots) {
        if(!missing(nknots) && !is.null(nknots))
            warning("'all.knots' is TRUE; 'nknots' specification is disregarded")
        nknots <- nx
    } else if(is.null(nknots))# <- for back compatibility
	nknots <- .nknots.smspl(nx)
    else {
	if(is.function(nknots))
	    nknots <- nknots(nx)
	else if(!is.numeric(nknots))
	    stop("'nknots' must be numeric (in {1,..,n})")
	if(nknots < 1)
	    stop("'nknots' must be at least 1")
	else if(nknots > nx)
	    stop("cannot use more inner knots than unique 'x' values")
    }
    knot <- c(rep(xbar[1 ], 3),
              if(all.knots) xbar else xbar[seq.int(1, nx, length.out = nknots)],
              rep(xbar[nx], 3))
    nk <- nknots + 2L ## == length(knot) - 4

    ## ispar != 1 : compute spar (later)
    ispar <-
        if(is.null(spar) || missing(spar)) { ## || spar == 0
            if(contr.sp$trace) -1L else 0L
        } else 1L
    spar <- if(ispar == 1L) as.double(spar) else double(1)
    ## was <- if(missing(spar)) 0 else if(spar < 1.01e-15) 0 else  1
    ## but package forecast passed a length-0 vector.
    if(length(spar) != 1) stop("'spar' must be of length 1")

    ## icrit {../src/sslvrg.f}:
    ##		(0 = no crit,  1 = GCV ,  2 = ord.CV , 3 = df-matching)
    icrit <- if(is.na(cv)) 0L else if(cv) 2L else 1L
    dofoff <- df.offset
    if(!missing(df)) { # not when cv was NA
	if(df > 1 && df <= nx) {
	    if(!missing(cv))
		warning("specified both 'df' and 'cv'; will disregard the latter")
	    icrit <- 3L
	    dofoff <- df
	} else warning("you must supply 1 < df <= n,  n = #{unique x} = ", nx)
    }
    iparms <- c(icrit=icrit, ispar=ispar, iter=as.integer(contr.sp$maxit))

    keep.stuff <- FALSE ## << to become an argument in the future
    ans.names <- c("coef","ty","lev","spar","parms","crit","iparms","ier",
                   if(keep.stuff) "scratch")
    fit <- .Fortran(C_rbart,		# code in ../src/qsbart.f
		    as.double(penalty),
		    as.double(dofoff),
		    x = as.double(xbar),
		    y = as.double(ybar),
		    w = as.double(wbar), # changed in the Fortran code
		    ssw = as.double(yssw),
		    as.integer(nx),
		    as.double(knot),
		    as.integer(nk),
		    coef = double(nk),
		    ty = double(nx),
		    lev = double(if(is.na(cv))1L else nx),
		    crit = double(1),
		    iparms = iparms,
		    spar = spar,
		    parms = unlist(contr.sp[1:4]),
		    scratch = double(17L * nk + 1L),
		    ld4  = 4L,
		    ldnk = 1L,
		    ier = integer(1L)
		    )[ans.names]
    ## now we have clobbered wbar, recompute it.
    wbar <- tmp[, 1]

    if(is.na(cv)) lev <- df <- NA
    else {
	lev <- fit$lev
	df <- sum(lev)
	if(is.na(df))
	    stop("NA lev[]; probably smoothing parameter 'spar' way too large!")
    }
    if(fit$ier > 0L ) {
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
	if(is.na(cv)) NA
	else if(cv) {
	    ww <- wbar
	    ww[!(ww > 0)] <- 1
	    weighted.mean(((y - fit$ty[ox])/(1 - (lev[ox] * w)/ww[ox]))^2, w)
	} else weighted.mean((y - fit$ty[ox])^2, w)/
	    (1 - (df.offset + penalty * df)/n)^2
    pen.crit <- sum(wbar * (ybar - fit$ty)^2)
    fit.object <- list(knot = knot, nk = nk, min = ux[1L], range = r.ux,
		       coef = fit$coef)
    class(fit.object) <- "smooth.spline.fit"
    ## parms :  c(low = , high = , tol = , eps = )
    object <- list(x = ux, y = fit$ty, w = wbar, yin = ybar,
                   data = if(keep.data) list(x = x, y = y, w = w),
		   lev = lev, cv.crit = cv.crit, pen.crit = pen.crit,
                   crit = fit$crit,
                   df = df, spar = fit$spar,
                   lambda = unname(fit$parms["low"]),
                   iparms = fit$iparms, # c(icrit= , ispar= , iter= )
                   fit = fit.object, call = match.call())
    class(object) <- "smooth.spline"
    object
}

fitted.smooth.spline <- function(object, ...) {
    if(!is.list(dat <- object$data))
        stop("need result of smooth.spline(keep.data = TRUE)")
    ## note that object$x == unique(sort(object$data$x))
    object$y[match(dat$x, object$x)]
}

residuals.smooth.spline <-
    function (object, type = c("working", "response", "deviance",
                      "pearson", "partial"), ...)
{
    type <- match.arg(type)
    if(!is.list(dat <- object$data))
        stop("need result of smooth.spline(keep.data = TRUE)")
    r <- dat$y - object$y[match(dat$x, object$x)]
    ## this rest is `as' residuals.lm() :
    res <- switch(type,
                  working = ,
                  response = r,
                  deviance = ,
                  pearson = if (is.null(dat$w)) r else r * sqrt(dat$w),
                  partial = r)
    res <- naresid(object$na.action, res)
    if (type == "partial")
        stop('type = "partial" is not yet implemented')
        ## res <- res + predict(object, type = "terms")
    res
}


print.smooth.spline <- function(x, digits = getOption("digits"), ...)
{
    if(!is.null(cl <- x$call)) {
	cat("Call:\n")
	dput(cl, control=NULL)
    }
    ip <- x$iparms
    cv <- cl$cv
    if(is.null(cv)) cv <- FALSE else if(is.name(cv)) cv <- eval(cv)
    cat("\nSmoothing Parameter  spar=", format(x$spar, digits=digits),
        " lambda=", format(x$lambda, digits=digits),
        if(ip["ispar"] != 1L) paste0("(", ip["iter"], " iterations)"))
    cat("\n")
    cat("Equivalent Degrees of Freedom (Df):", format(x$df,digits=digits))
    cat("\n")
    cat("Penalized Criterion:", format(x$pen.crit, digits=digits))
    cat("\n")
    if(!is.na(cv))
        cat(if(cv) "PRESS: " else "GCV: ",
            format(x$cv.crit, digits = digits), "\n", sep = "")
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
	x <- seq.int(from = object$min, to = object$min + object$range,
                     length.out = length(object$coef) - 4L)
    xs <- (x - object$min)/object$range # x scaled to [0,1]
    extrap.left <- xs < 0
    extrap.right <- xs > 1
    interp <- !(extrap <- extrap.left | extrap.right)
    n <- sum(interp) # number of xs in [0,1]
    y <- xs
    if(any(interp))
	y[interp] <- .Fortran(C_bvalus,
			      n	  = as.integer(n),
			      knot = as.double(object$knot),
			      coef = as.double(object$coef),
			      nk = as.integer(object$nk),
			      x	= as.double(xs[interp]),
			      s	= double(n),
			      order = as.integer(deriv))$s
    if(any(extrap)) {
	xrange <- c(object$min, object$min + object$range)
	if(deriv == 0) {
	    end.object <- Recall(object, xrange)$y
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    if(any(extrap.left))
		y[extrap.left] <- end.object[1L] +
		    end.slopes[1L] * (xs[extrap.left] - 0)
	    if(any(extrap.right))
		y[extrap.right] <- end.object[2L] +
		    end.slopes[2L] * (xs[extrap.right] - 1)
	} else if(deriv == 1) {
	    end.slopes <- Recall(object, xrange, 1)$y * object$range
	    y[extrap.left] <- end.slopes[1L]
	    y[extrap.right] <- end.slopes[2L]
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
	iper <- 2L
	xrange <- range(x)
	if(xrange[1L] < 0 || xrange[2L] > 1)
	    stop("'x' must be between 0 and 1 for periodic smooth")
    } else iper <- 1L
    okay <- is.finite(x + y + wt)
    ord <- order(x[okay], y[okay])
    ord <- cumsum(!okay)[okay][ord] + ord
    xo <- x[ord]
    leno <- length(ord)
    if(leno == 0L)
        stop("no finite observations")
    if(diff <- n - leno)
        warning(sprintf(ngettext(diff,
                                 "%d observation with NA, NaN or Inf deleted",
                                 "%d observations with NAs, NaNs and/or Infs deleted"),
                        diff), domain = NA)
    .Fortran(C_setsmu)
    smo <- .Fortran(C_supsmu,
		    as.integer(leno),
		    as.double(xo),
		    as.double(y[ord]),
		    as.double(wt[ord]),
		    as.integer(iper),
		    as.double(span),
		    as.double(bass),
		    smo=double(leno),
		    double(n*7L), double(1L))$smo
    ## eliminate duplicate xsort values and corresponding smoothed values
    dupx <- duplicated(xo)
    list(x = xo[!dupx], y = smo[!dupx])
}
