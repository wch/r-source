#  File src/library/splines/R/splineClasses.R
#  Part of the R package, https://www.R-project.org
#  Copyright (C) 2000-2018 The R Core Team
#  Copyright (C) 1998 Douglas M. Bates and William N. Venables.
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

#### Classes and methods for determining and manipulating interpolation
#### splines.

### Major classes:
###   spline - a virtual class of representations of piecewise
###	       polynomial functions.  The join points of the polynomials
###	       are called "knots".  The order of the spline is the number
###	       of coefficients in the polynomial pieces.
###   bSpline - splines represented as linear combinations of B-splines
###   polySpline - splines represented as polynomials

### Minor classes:
###   nbSpline - "natural" bSplines.  That is, splines of order 4 with linear
###		 extrapolation beyond the limits of the knots.
###   npolySpline - polynomial representation of a natural spline
###   pbSpline - periodic bSplines
###   ppolySpline - periodic polynomial splines
###   backSpline - "splines" for inverse interpolation

splineDesign <-
    ## Creates the "design matrix" for a collection of B-splines.
    function(knots, x, ord = 4L, derivs = 0L, outer.ok = FALSE,
             sparse = FALSE)
{
    if((nk <- length(knots <- as.numeric(knots))) <= 0)
        stop("must have at least 'ord' knots")
    if(is.unsorted(knots)) knots <- sort.int(knots)
    x <- as.numeric(x)
    nx <- length(x)
    ## derivs is re-cycled to length(x) in C
    if(length(derivs) > nx)
	stop("length of 'derivs' is larger than length of 'x'")
    if(length(derivs) < 1L) stop("empty 'derivs'")
    ord <- as.integer(ord)
    if(ord > nk || ord < 1)
	stop("'ord' must be positive integer, at most the number of knots")

    ## The x test w/ sorted knots assumes ord <= nk+1-ord, or nk >= 2*ord-1L:
    if(!outer.ok && nk < 2*ord-1)
        stop(gettextf("need at least %s (=%d) knots",
                      "2*ord -1", 2*ord -1),
             domain = NA)

    degree <- ord - 1L
### FIXME: the 'outer.ok && need.outer' handling would more efficiently happen
###        in the underlying C code - with some programming effort though..
    if(need.outer <- any(x < knots[ord] | knots[nk - degree] < x)) {
        if(outer.ok) { ## x[] is allowed to be 'anywhere'
	    in.x <- knots[1L] <= x & x <= knots[nk]
	    if((x.out <- !all(in.x))) {
		x <- x[in.x]
		nnx <- length(x)
	    }
	    ## extend knots set "temporarily": the boundary knots must be repeated >= 'ord' times.
            ## NB: If these are already repeated originally, then, on the *right* only, we need
            ##    to make sure not to add more than needed
            dkn <- diff(knots)[(nk-1L):1] # >= 0, since they are sorted
	    knots <- knots[c(rep.int(1L, degree),
                             seq_len(nk),
                             rep.int(nk, max(0L, ord - match(TRUE, dkn > 0))))]
	} else
	    stop(gettextf("the 'x' data must be in the range %g to %g unless you set '%s'",
			  knots[ord], knots[nk - degree], "outer.ok = TRUE"),
		 domain = NA)
    }
    temp <- .Call(C_spline_basis, knots, ord, x, derivs)
    ncoef <- nk - ord

    ii <- if(need.outer && x.out) { # only assign non-zero for x[]'s "inside" knots
        rep.int((1L:nx)[in.x], rep.int(ord, nnx))
    } else rep.int(1L:nx, rep.int(ord, nx))
    jj <- c(outer(1L:ord, attr(temp, "Offsets"), "+"))
    ## stopifnot(length(ii) == length(jj))

    if(sparse) {
	if(is.null(tryCatch(loadNamespace("Matrix"), error = function(e)NULL)))
	    stop(gettextf("%s needs package 'Matrix' correctly installed",
                          "splineDesign(*, sparse=TRUE)"),
                 domain = NA)
	if(need.outer) { ## shift column numbers and drop those "outside"
	    jj <- jj - degree - 1L
	    ok <- 0 <= jj & jj < ncoef
	    methods::as(methods::new("dgTMatrix", i = ii[ok] - 1L, j = jj[ok],
				     x = as.double(temp[ok]), # vector, not matrix
				     Dim = c(nx, ncoef)), "CsparseMatrix")
	}
	else
	    methods::as(methods::new("dgTMatrix", i = ii - 1L, j = jj - 1L,
				     x = as.double(temp), # vector
				     Dim = c(nx, ncoef)), "CsparseMatrix")
    } else { ## traditional (dense) matrix
	design <- matrix(double(nx * ncoef), nx, ncoef)
	if(need.outer) { ## shift column numbers and drop those "outside"
	    jj <- jj - degree
	    ok <- 1 <= jj & jj <= ncoef
	    design[cbind(ii, jj)[ok , , drop=FALSE]] <- temp[ok]
	}
	else
	    design[cbind(ii, jj)] <- temp
	design
    }
}

interpSpline <-
    ## Determine the natural interpolation spline.
    function(obj1, obj2, bSpline = FALSE, period = NULL, ord = 4L,
             na.action = na.fail, sparse = FALSE)
    UseMethod("interpSpline")

##>>> FIXME: any  ord != 4 needs adaption in splineDesign(), i.e.,
##>>> =====       --------  probably in spline_basis() in ../src/splines.c
interpSpline.default <-
    function(obj1, obj2, bSpline = FALSE, period = NULL,
             ord = 4L, na.action = na.fail, sparse = FALSE)
{
    ## spline order 'ord' == 'degree' + 1
    stopifnot(exprs = {
        (degree <- ord - 1L) >= 0
        length(degree) == 1L
        degree == as.integer(degree)
    })
    frm <- na.action(data.frame(x = as.numeric(obj1), y = as.numeric(obj2)))
    frm <- frm[order(frm$x), ]
    ndat <- nrow(frm)
    x <- frm$x
    if(anyDuplicated(x))
	stop("values of 'x' must be distinct")
    if(length(x) < ord)
        stop(gettextf("must have at least 'ord'=%d points", ord), domain=NA)
    ## 'degree' extra knots (shifted) out on each side :
    iDeg <- seq_len(degree)
    knots <- c(x[iDeg] + x[1L] - x[ord], x,
	       x[ndat + iDeg - degree] + x[ndat] - x[ndat - degree])
    if(even <- (ord %% 2 == 0)) { ## natural boundary conditions:
	nu <- ord %/% 2L          ## nu-th derivs coerced to 0  [in solve() below]
	derivs <- c(nu, integer(ndat),  nu)
	x      <- c(x[1L],   x,    x[ndat])
    }
## Solving the system of equations for the spline coefficients can be
## simplified by using banded matrices but the required LINPACK routines
## are not loaded as part of S.
##  z <- .C("spline_basis",
##	as.double(knots),
##	as.integer(length(knots) - 4),
##	as.integer(4),
##	as.double(x),
##	as.integer(derivs),
##	as.integer(ndat + 2),
##	design = array(0, c(4, ndat + 2)),
##	offsets = integer(ndat + 2))
##  abd <- array(0, c(7, ndat + 2))
##  abd[4:7, 2:ndat] <- z$design[, 2:ndat]
##  abd[5:7, 1] <- z$design[-4, 1]
##  abd[4:6, ndat + 1] <- z$design[-1, ndat + 1]
##  abd[3:5, ndat + 2] <- z$design[-1, ndat + 2]
##  z <- .Fortran("dgbfa",
##	abd = abd,
##	lda = as.integer(7),
##	n = as.integer(ndat + 2),
##	ml = 2L,
##	mu = 2L,
##	ipvt = integer(ndat + 2),
##	info = integer(1))
##  zz <- .Fortran("dgbsl",
##	abd = z$abd,
##	lda = z$lda,
##	n = z$n,
##	ml = z$ml,
##	mu = z$mu,
##	ipvt = z$ipvt,
##	b = c(0, y, 0),
##	job = 1L)
    des <- splineDesign(knots, x, ord, derivs, sparse=sparse)
    y <- c(0, frm$y, 0)
    coeff <- if(sparse) Matrix::solve(des, Matrix::..2dge(y), sparse=TRUE)
             else solve(des, y)
    value <- structure(
        list(knots = knots, coefficients = coeff, order = ord),
             formula = do.call("~", list(substitute(obj2), substitute(obj1))),
        class = c("nbSpline", "bSpline", "spline"))
    if (bSpline) return(value)
    ## else convert from B- to poly-Spline:
    value <- polySpline(value)
    coeff <- coef(value)
    coeff[ , 1L] <- frm$y
    coeff[1L, degree] <- coeff[nrow(coeff), degree] <- 0
    value$coefficients <- coeff
    value
}

interpSpline.formula <-
    function(obj1, obj2, bSpline = FALSE, period = NULL,
             ord = 4L, na.action = na.fail, sparse = FALSE)
{
    form <- as.formula(obj1)
    if (length(form) != 3)
	stop("'formula' must be of the form \"y ~ x\"")
    local <- if (missing(obj2)) sys.parent(1) else as.data.frame(obj2)
    value <- interpSpline(as.numeric(eval(form[[3L]], local)),
			  as.numeric(eval(form[[2L]], local)),
			  bSpline=bSpline, period=period, ord=ord,
			  na.action=na.action, sparse=sparse)
    attr(value, "formula") <- form
    value
}

periodicSpline <-
    ## Determine the periodic interpolation spline.
    function(obj1, obj2, knots, period = 2 * pi, ord = 4L)
    UseMethod("periodicSpline")

periodicSpline.default <-
    function(obj1, obj2, knots, period = 2 * pi, ord = 4L)
{
    x <- as.numeric(obj1)
    y <- as.numeric(obj2)
    lenx <- length(x)
    if(lenx != length(y))
	stop("lengths of 'x' and 'y' must match")
    ind <- order(x)
    x <- x[ind]
    if(length(unique(x)) != lenx)
	stop("values of 'x' must be distinct")
    if(any((x[-1L] - x[ - lenx]) <= 0))
	stop("values of 'x' must be strictly increasing")
    if(ord < 2) stop("'ord' must be >= 2")
    degree <- ord - 1L
    if(!missing(knots)) {
	period <- knots[length(knots) - degree] - knots[1L]
    }
    else {
	knots <- c(x[(lenx - (ord - 2)):lenx] - period, x, x[1L:ord] + period)
    }
    if((x[lenx] - x[1L]) >= period)
	stop("the range of 'x' values exceeds one period")
    y <- y[ind]
    coeff.mat <- splineDesign(knots, x, ord)
    i1 <- seq_len(degree)
    sys.mat <- coeff.mat[, (1L:lenx)]
    sys.mat[, i1] <- sys.mat[, i1] + coeff.mat[, lenx + i1]
    coeff <- qr.coef(qr(sys.mat), y)
    coeff <- c(coeff, coeff[i1])
    structure(list(knots = knots, coefficients = coeff,
		   order = ord, period = period),
	      formula = do.call("~", as.list(sys.call())[3:2]),
	      class = c("pbSpline", "bSpline", "spline"))
}

periodicSpline.formula <- function(obj1, obj2, knots, period = 2 * pi, ord = 4L)
{
    form <- as.formula(obj1)
    if (length(form) != 3)
	stop("'formula' must be of the form \"y ~ x\"")
    local <- if (missing(obj2)) sys.parent(1) else as.data.frame(obj2)
    ## 'missing(knots)' is transfered :
    structure(periodicSpline.default(as.numeric(eval(form[[3L]], local)),
				     as.numeric(eval(form[[2L]], local)),
				     knots = knots, period = period, ord = ord),
	      formula = form)
}

polySpline <-
    ## Constructor for polynomial representation of splines
    function(object, ...) UseMethod("polySpline")

polySpline.polySpline <- function(object, ...) object

as.polySpline <-
    ## Conversion of an object to a polynomial spline representation
    function(object, ...) polySpline(object, ...)

polySpline.bSpline <- function(object, ...)
{
    ord <- splineOrder(object)
    knots <- splineKnots(object)
    if(is.unsorted(knots))
	stop("knot positions must be non-decreasing")
    knots <- knots[ord:(length(knots) + 1L - ord)]
    coeff <- array(0, c(length(knots), ord))
    coeff[, 1] <- asVector(predict(object, knots))
    if(ord > 1) {
	for(i in 2:ord) {
	    coeff[, i] <- asVector(predict(object, knots, deriv = i - 1L))/
		prod(seq_len(i - 1L))
	}
    }
    structure(list(knots = knots, coefficients = coeff),
	      formula = attr(object, "formula"),
	      class = c("polySpline", "spline"))
}

polySpline.nbSpline <- function(object, ...)
{
    structure(NextMethod("polySpline"),
              class = c("npolySpline", "polySpline", "spline"))
}

polySpline.pbSpline <- function(object, ...)
{
    value <- NextMethod("polySpline")
    value[["period"]] <- object$period
    class(value) <- c("ppolySpline", "polySpline", "spline")
    value
}

## A couple of accessor functions for the virtual class of splines.

splineKnots <- ## Extract the knot positions
    function(object) UseMethod("splineKnots")

splineKnots.spline <- function(object) object$knots

splineOrder <- ## Extract the order of the spline
    function(object) UseMethod("splineOrder")

splineOrder.bSpline <- function(object) object$order

splineOrder.polySpline <- function(object) ncol(coef(object))

## xyVector is a class of numeric vectors that represent responses and
## carry with them the corresponding inputs x.	Very similar in purpose
## to the "track" class in JMC's book.  All methods for predict
## applied to spline objects produce such objects as their value.

xyVector <- ## Constructor for the xyVector class
    function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    if(length(x) != length(y))
	stop("lengths of 'x' and 'y' must be the same")
    structure(list(x = x, y = y), class = "xyVector")
}

asVector <- ## coerce object to a vector.
    function(object) UseMethod("asVector")

asVector.xyVector <- function(object) object$y

as.data.frame.xyVector <- function(x, ...) data.frame(x = x$x, y = x$y)

plot.xyVector <- function(x, ...)
{
    plot(x = x$x, y = x$y, ...)
###  xyplot(y ~ x, as.data.frame(x), ...)
}

predict.polySpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    knots <- splineKnots(object)
    coeff <- coef(object)
    cdim <- dim(coeff)
    ord <- cdim[2L]
    if(missing(x))
	x <- seq.int(knots[1L], knots[cdim[1L]], length.out = nseg + 1)
    i <- as.numeric(cut(x, knots))
    i[x == knots[1L]] <- 1
    delx <- x - knots[i]
    deriv <- as.integer(deriv)[1L]
    if(deriv < 0 || deriv >= ord)
	stop(gettextf("'deriv' must be between 0 and %d", ord - 1),
             domain = NA)
    while(deriv > 0) {
	ord <- ord - 1L
	coeff <- t(t(coeff[, -1]) * seq_len(ord))
	deriv <- deriv - 1L
    }
    y <- coeff[i, ord]
    if(ord > 1) {
	for(j in (ord - 1L):1)
	    y <- y * delx + coeff[i, j]
    }
    xyVector(x = x, y = y)
}

predict.bSpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    knots <- splineKnots(object)
    if(is.unsorted(knots))
	stop("knot positions must be non-decreasing")
    ord <- splineOrder(object)
    if(deriv < 0 || deriv >= ord)
	stop(gettextf("'deriv' must be between 0 and %d", ord - 1),
             domain = NA)
    ncoeff <- length(coeff <- coef(object))
    if(missing(x)) {
	x <- seq.int(knots[ord], knots[ncoeff + 1], length.out = nseg + 1)
	accept <- TRUE
    } else accept <- knots[ord] <= x & x <= knots[ncoeff + 1]
    y <- x
    y[!accept] <- NA # (C_spline_value's FIXME)
    y[accept] <- .Call(C_spline_value, knots, coeff, ord, x[accept], deriv)
    xyVector(x = x, y = y)
}

predict.nbSpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    value <- NextMethod("predict") # predict.bSpline() -> NaN outside knots
    if(!any(is.na(value$y))) # when x were inside knots
	return(value)

    x <- value$x
    y <- value$y
    ## Compute y[] for x[] outside knots:
    knots <- splineKnots(object)
    ord <- splineOrder(object)
    ncoeff <- length(coef(object))
    bKn <- knots[c(ord,ncoeff + 1)]
    coeff <- array(0, c(2L, ord))
    ## Extrapolate using a + b*(x - boundary.knot) (ord=4 specific?)
    coeff[,1] <- asVector(predict(object, bKn))
    coeff[,2] <- asVector(predict(object, bKn, deriv = 1))
    deriv <- as.integer(deriv)## deriv < ord already tested in NextMethod()
    while(deriv) {
	ord <- ord - 1
	## could be simplified when coeff has <= 2 non-zero cols:
	coeff <- t(t(coeff[, -1]) * (1L:ord))# 1L:ord = the 'k' in k* x^{k-1}
	deriv <- deriv - 1
    }
    nc <- ncol(coeff)
    if(any(which <- (x < bKn[1L]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[1, 1] + coeff[1, 2] * (x[which] - bKn[1L])
    if(any(which <- (x > bKn[2L]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[2, 1] + coeff[2, 2] * (x[which] - bKn[2L])
    xyVector(x = x, y = y)
}

predict.pbSpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    knots <- splineKnots(object)
    ord <- splineOrder(object)
    period <- object$period
    ncoeff <- length(coef(object))
    if(missing(x))
	x <- seq.int(knots[ord], knots[ord] + period, length.out = nseg + 1)
    ## Because of C_spline_value's FIXME, we move the outside-knots x[] values:
    x.original <- x
    if(any(ind <- x < knots[ord]))
	x[ind] <- x[ind] + period * (1 + (knots[ord] - x[ind]) %/% period)
    if(any(ind <- x > knots[ncoeff + 1]))
	x[ind] <- x[ind] - period * (1 + (x[ind] - knots[ncoeff +1]) %/% period)
    xyVector(x = x.original,
	     y = .Call(C_spline_value, knots, coef(object), ord, x, deriv))
}

predict.npolySpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    value <- NextMethod() # typically predict.polySpline()
    if(!any(is.na(value$y))) # when x were inside knots
	return(value)

    x <- value$x
    y <- value$y
    ## Compute y[] for x[] outside knots:
    nk <- length(knots <- splineKnots(object))
    coeff <- coef(object)[ - (2:(nk - 1)), ] # only need col 1L:2
    ord <- dim(coeff)[2L]
    if(ord >= 3) coeff[, 3:ord] <- 0
    deriv <- as.integer(deriv)
    while(deriv) {
	ord <- ord - 1
	## could be simplified when coeff has <= 2 non-zero cols:
	coeff <- t(t(coeff[, -1]) * (1L:ord))# 1L:ord = the 'k' in k* x^{k-1}
	deriv <- deriv - 1
    }
    nc <- ncol(coeff)
    if(any(which <- (x < knots[1L]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[1, 1] + coeff[1, 2] * (x[which] - knots[1L])
    if(any(which <- (x > knots[nk]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[2, 1] + coeff[2, 2] * (x[which] - knots[nk])

    xyVector(x = x, y = y)
}

predict.ppolySpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    knots <- splineKnots(object)
    nknot <- length(knots)
    period <- object$period
    if(missing(x))
	x <- seq.int(knots[1L], knots[1L] + period, length.out = nseg + 1)
    x.original <- x

    if(any(ind <- x < knots[1L]))
	x[ind] <- x[ind] + period * (1 + (knots[1L] - x[ind]) %/% period)
    if(any(ind <- x > knots[nknot]))
	x[ind] <- x[ind] - period * (1 + (x[ind] - knots[nknot]) %/% period)

    value <- NextMethod("predict")
    value$x <- x.original
    value
}

## The plot method for all spline objects

plot.spline <- function(x, ...)
{
###  args <- list(formula = y ~ x, data = as.data.frame(predict(x)), type = "l")
    args <- list(x = as.data.frame(predict(x)), type = "l")
    if(length(form <- attr(x, "formula")) == 3) {
	args <- c(args, list(xlab = deparse(form[[3L]]), ylab = deparse(form[[2L]])))
    }
    args <- c(list(...), args)
###  do.call("xyplot", args)
    do.call("plot", args[unique(names(args))])
}

print.polySpline <- function(x, ...)
{
    coeff <- coef(x)
    dnames <- dimnames(coeff)
    if (is.null(dnames[[2L]]))
	dimnames(coeff) <-
	    list(format(splineKnots(x)),
		 c("constant", "linear", "quadratic", "cubic",
		   paste0(4:29, "th"))[1L:(dim(coeff)[2L])])
    cat("polynomial representation of spline")
    if (!is.null(form <- attr(x, "formula")))
	cat(" for", deparse(as.vector(form)))
    cat("\n")
    print(coeff, ...)
    invisible(x)
}

print.ppolySpline <- function(x, ...)
{
    cat("periodic ")
    value <- NextMethod("print")
    cat("\nPeriod:", format(x[["period"]]), "\n")
    value
}

print.bSpline <- function(x, ...)
{
    value <- c(rep(NA, splineOrder(x)), coef(x))
    names(value) <- format(splineKnots(x), digits = 5)
    cat("bSpline representation of spline")
    if (!is.null(form <- attr(x, "formula")))
	cat(" for", deparse(as.vector(form)))
    cat("\n")
    print(value, ...)
    invisible(x)
}

## backSpline - a class of monotone inverses to an interpolating spline.
## Used mostly for the inverse of the signed square root profile function.

backSpline <- function(object) UseMethod("backSpline")

backSpline.npolySpline <- function(object)
{
    knots <- splineKnots(object)
    nk <- length(knots)
    nkm1 <- nk - 1
    kdiff <- diff(knots)
    if(any(kdiff <= 0))
	stop("knot positions must be strictly increasing")
    coeff <- coef(object)
    if((ord <- ncol(coeff)) != 4)
	stop("currently implemented only for cubic splines")
    bknots <- coeff[, 1]
    adiff <- diff(bknots)
    if(all(adiff < 0))
	revKnots <- TRUE
    else if(all(adiff > 0))
	revKnots <- FALSE
    else
	stop("spline must be monotone")
    bcoeff <- array(0, dim(coeff))
    bcoeff[, 1] <- knots
    bcoeff[, 2] <- 1/coeff[, 2]
    a <- array(c(adiff^2, 2 * adiff, adiff^3, 3 * adiff^2),
	       c(nkm1, 2L, 2L))
    b <- array(c(kdiff - adiff * bcoeff[ - nk, 2L],
		 bcoeff[-1L, 2L] - bcoeff[ - nk, 2L]), c(nkm1, 2))
    for(i in 1L:(nkm1))
	bcoeff[i, 3L:4L] <- solve(a[i,, ], b[i,  ])
    bcoeff[nk, 2L:4L] <- NA
    if(nk > 2L) {
	bcoeff[1L, 4L] <- bcoeff[nkm1, 4L] <- 0
	bcoeff[1L, 2L:3L] <- solve(array(c(adiff[1L], 1, adiff[1L]^2,
                                           2 * adiff[1L]), c(2L, 2L)),
                                   c(kdiff[1L], 1/coeff[2L, 2L]))
	bcoeff[nkm1, 3L] <- (kdiff[nkm1] - adiff[nkm1] *
			    bcoeff[nkm1, 2L])/adiff[nkm1]^2
    }
    if(bcoeff[1L, 3L] > 0) {
	bcoeff[1L, 3L] <- 0
	bcoeff[1L, 2L] <- kdiff[1L]/adiff[1L]
    }
    if(bcoeff[nkm1, 3L] < 0) {
	bcoeff[nkm1, 3L] <- 0
	bcoeff[nkm1, 2L] <- kdiff[nkm1]/adiff[nkm1]
    }
    value <- if(!revKnots) list(knots = bknots, coefficients = bcoeff)
    else {
	ikn <- length(bknots):1L
	list(knots = bknots[ikn], coefficients = bcoeff[ikn,])
    }
    attr(value, "formula") <- do.call("~", as.list(attr(object, "formula"))[3L:2L])
    class(value) <- c("polySpline", "spline")
    value
}

backSpline.nbSpline <- function(object) backSpline(polySpline(object))
