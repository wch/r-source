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

##     Copyright (C) 1998 Douglas M. Bates and William N. Venables.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.

splineDesign <-
    ## Creates the "design matrix" for a collection of B-splines.
    function(knots, x, ord = 4, derivs = integer(nx), outer.ok = FALSE)
{
    knots <- sort(as.numeric(knots))
    if((nk <- length(knots)) <= 0) stop("must have at least 'ord' knots")
    x <- as.numeric(x)
    nx <- length(x)
    if(length(derivs) != nx)
	stop("length of 'derivs' must match length of 'x'")
    if(ord > nk || ord < 1)
	stop("'ord' must be positive integer, at most the number of knots")

    ## The x test w/ sorted knots assumes ord <= nk+1-ord, or nk >= 2*ord-1:
    if(!outer.ok && nk < 2*ord-1)
        stop(gettextf("need at least 2*ord -1 (=%d) knots", 2*ord -1),
             domain = NA)

    o1 <- ord - 1
### FIXME: the 'outer.ok && need.outer' handling would more efficiently happen
###        in the underlying C code - with some programming effort though..
    if(need.outer <- any(out.x <- x < knots[ord] | knots[nk - o1] < x)) {
        if(outer.ok) { ## x[] is allowed to be 'anywhere'
            ## extend knots set "temporarily"
            in.x <- knots[1] < x & x < knots[nk]
	    knots <- knots[c(rep.int(1L, o1), 1:nk, rep.int(nk, o1))]
            if((x.out <- !all(in.x))) {
                x <- x[in.x]
                nnx <- length(x)
            }
	} else
        stop(gettextf("the 'x' data must be in the range %g to %g unless you set 'outer.ok = TRUE'", knots[ord], knots[nk- o1]), domain = NA)
    }
    temp <- .Call("spline_basis", knots, ord, x, derivs, PACKAGE = "splines")
    ncoef <- nk - ord
    design <- matrix(double(nx * ncoef), nx, ncoef)

    ii <- if(need.outer && x.out) { # only assign non-zero for x[]'s "inside" knots
        rep.int((1:nx)[in.x], rep.int(ord, nnx))
    } else rep.int(1:nx, rep.int(ord, nx))
    jj <- c(outer(1:ord, attr(temp, "Offsets"), "+"))
    ## stopifnot(length(ii) == length(jj))

    if(need.outer) { ## shift column numbers and drop those "outside"
        jj <- jj - o1
        ok <- 1 <= jj & jj <= ncoef
	design[cbind(ii, jj)[ok , , drop=FALSE]] <- temp[ok]
    }
    else
        design[cbind(ii, jj)] <- temp

    design
}

interpSpline <-
    ## Determine the natural interpolation spline.
    function(obj1, obj2, bSpline = FALSE, period = NULL, na.action = na.fail)
    UseMethod("interpSpline")

interpSpline.default <-
    function(obj1, obj2, bSpline = FALSE, period = NULL, na.action = na.fail)
{
    ord <- 4 # spline order -- future: want to use other 'order'/'degree'!
    deg <- ord - 1

    frm <- na.action(data.frame(x = as.numeric(obj1), y = as.numeric(obj2)))
    frm <- frm[order(frm$x), ]
    ndat <- nrow(frm)
    x <- frm$x
    if(any(duplicated(x)))
	stop("values of 'x' must be distinct")
    ## 'deg' extra knots (shifted) out on each side :
    knots <- c(x[1:deg] + x[1] - x[ord], x,
	       x[ndat + (1:deg) - deg] + x[ndat] - x[ndat - deg])
    derivs <- c(2, integer(ndat), 2) # 2nd derivs coerced to 0 in solve() below
    x	   <- c(x[1], x, x[ndat])
## Solving the system of equations for the spline coefficients can be
## simplified by using banded matrices but the required Linpack routines
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
##	ml = as.integer(2),
##	mu = as.integer(2),
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
##	job = as.integer(1))
    des <- splineDesign(knots, x, ord, derivs)
    coeff <- solve(as.matrix(des), c(0, frm$y, 0))
    value <- list(knots = knots, coefficients = coeff, order = ord)
    attr(value, "formula") <-
	do.call("~", list(substitute(obj2), substitute(obj1)))
    class(value) <- c("nbSpline", "bSpline", "spline")
    if (bSpline) return(value)
    ## else convert from B- to poly-Spline:
    value <- polySpline(value)
    coeff <- coef(value)
    coeff[ , 1] <- frm$y
    coeff[1, deg] <- coeff[nrow(coeff), deg] <- 0
    value$coefficients <- coeff
    value
}

interpSpline.formula <-
    function(obj1, obj2, bSpline = FALSE, period = NULL, na.action = na.fail)
{
    form <- as.formula(obj1)
    if (length(form) != 3)
	stop("'formula' must be of the form \"y ~ x\"")
    local <- if (missing(obj2)) sys.parent(1) else as.data.frame(obj2)
    value <- interpSpline(as.numeric(eval(form[[3]], local)),
			  as.numeric(eval(form[[2]], local)),
			  bSpline = bSpline, period = period,
			  na.action = na.action)
    attr(value, "formula") <- form
    value
}

periodicSpline <-
    ## Determine the periodic interpolation spline.
    function(obj1, obj2, knots, period = 2 * pi, ord = 4)
    UseMethod("periodicSpline")

periodicSpline.default <-
    function(obj1, obj2, knots, period = 2 * pi, ord = 4)
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
    if(any((x[-1] - x[ - lenx]) <= 0))
	stop("values of 'x' must be strictly increasing")
    if(ord < 2) stop("'ord' must be >= 2")
    if(!missing(knots)) {
	period <- knots[length(knots) + 1 - ord] - knots[1]
    }
    else {
	knots <- c(x[(lenx - (ord - 2)):lenx] - period, x, x[1:ord] + period)
    }
    if((x[lenx] - x[1]) >= period)
	stop("the range of 'x' values exceeds one period")
    y <- y[ind]
    coeff.mat <- splineDesign(knots, x, ord)
    sys.mat <- coeff.mat[, (1:lenx)]
    sys.mat[, 1:(ord - 1)] <- sys.mat[, 1:(ord - 1)] +
	coeff.mat[, lenx + (1:(ord - 1))]
    coeff <- qr.coef(qr(sys.mat), y)
    coeff <- c(coeff, coeff[1:(ord - 1)])
    value <- list(knots = knots, coefficients = coeff, order = ord,
		  period = period)
    attr(value, "formula") <- do.call("~", as.list(sys.call())[3:2])
    class(value) <- c("pbSpline", "bSpline", "spline")
    value
}

periodicSpline.formula <- function(obj1, obj2, knots, period = 2 * pi, ord = 4)
{
    form <- as.formula(obj1)
    if (length(form) != 3)
	stop("'formula' must be of the form \"y ~ x\"")
    local <- if (missing(obj2)) sys.parent(1) else as.data.frame(obj2)
    ## 'missing(knots)' is transfered :
    value <-  periodicSpline(as.numeric(eval(form[[3]], local)),
			     as.numeric(eval(form[[2]], local)),
			     knots = knots, period = period, ord = ord)
    attr(value, "formula") <- form
    value
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
    if(any(diff(knots) < 0))
	stop("knot positions must be non-decreasing")
    knots <- knots[ord:(length(knots) + 1 - ord)]
    coeff <- array(0, c(length(knots), ord))
    coeff[, 1] <- asVector(predict(object, knots))
    if(ord > 1) {
	for(i in 2:ord) {
	    coeff[, i] <- asVector(predict(object, knots, deriv = i - 1))/
		prod(1:(i - 1))
	}
    }
    value <- list(knots = knots, coefficients = coeff)
    attr(value, "formula") <- attr(object, "formula")
    class(value) <- c("polySpline", "spline")
    value
}

polySpline.nbSpline <- function(object, ...)
{
    value <- NextMethod("polySpline")
    class(value) <- c("npolySpline", "polySpline", "spline")
    value
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
## to the "track" class in JMC's book draft.  All methods for predict
## applied to spline objects produce such objects as their value.

xyVector <- ## Constructor for the xyVector class
    function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    if(length(x) != length(y)) {
	stop("lengths of 'x' and 'y' must be the same")
    }
    value <- list(x = x, y = y)
    class(value) <- "xyVector"
    value
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
    ord <- cdim[2]
    if(missing(x))
	x <- seq.int(knots[1], knots[cdim[1]], length.out = nseg + 1)
    i <- as.numeric(cut(x, knots))
    i[x == knots[1]] <- 1
    delx <- x - knots[i]
    deriv <- as.integer(deriv)[1]
    if(deriv < 0 || deriv >= ord)
	stop(gettextf("'deriv' must be between 0 and %d", ord - 1),
             domain = NA)
    while(deriv > 0) {
	ord <- ord - 1
	coeff <- t(t(coeff[, -1]) * (1:ord))
	deriv <- deriv - 1
    }
    y <- coeff[i, ord]
    if(ord > 1) {
	for(j in (ord - 1):1)
	    y <- y * delx + coeff[i, j]
    }
    xyVector(x = x, y = y)
}

predict.bSpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    knots <- splineKnots(object)
    if(any(diff(knots)) < 0)
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
    y[!accept] <- NA
    y[accept] <- .Call("spline_value", knots, coeff, ord, x[accept], deriv,
		       PACKAGE = "splines")
    xyVector(x = x, y = y)
}

predict.nbSpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    value <- NextMethod("predict")
    if(!any(is.na(value$y))) # when x were inside knots
	return(value)

    x <- value$x
    y <- value$y
    ## Compute y[] for x[] outside knots:
    knots <- splineKnots(object)
    ord <- splineOrder(object)
    ncoeff <- length(coef(object))
    bKn <- knots[c(ord,ncoeff + 1)]
    coeff <- array(0, c(2, ord))
    ## Extrapolate using a + b*(x - boundary.knot) (ord=4 specific?)
    coeff[,1] <- asVector(predict(object, bKn))
    coeff[,2] <- asVector(predict(object, bKn, deriv = 1))
    deriv <- as.integer(deriv)## deriv < ord already tested in NextMethod()
    while(deriv) {
	ord <- ord - 1
	## could be simplified when coeff has <= 2 non-zero cols:
	coeff <- t(t(coeff[, -1]) * (1:ord))# 1:ord = the 'k' in k* x^{k-1}
	deriv <- deriv - 1
    }
    nc <- ncol(coeff)
    if(any(which <- (x < bKn[1]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[1, 1] + coeff[1, 2] * (x[which] - bKn[1])
    if(any(which <- (x > bKn[2]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[2, 1] + coeff[2, 2] * (x[which] - bKn[2])
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
    x.original <- x
    if(any(ind <- x < knots[ord]))
	x[ind] <- x[ind] + period * (1 + (knots[ord] - x[ind]) %/% period)
    if(any(ind <- x > knots[ncoeff + 1]))
	x[ind] <- x[ind] - period * (1 + (x[ind] - knots[ncoeff +1]) %/% period)
    xyVector(x = x.original,
	     y = .Call("spline_value", splineKnots(object), coef(object),
	     splineOrder(object), x, deriv, PACKAGE = "splines"))
}

predict.npolySpline <- function(object, x, nseg = 50, deriv = 0, ...)
{
    value <- NextMethod()
    if(!any(is.na(value$y))) # when x were inside knots
	return(value)

    x <- value$x
    y <- value$y
    ## Compute y[] for x[] outside knots:
    nk <- length(knots <- splineKnots(object))
    coeff <- coef(object)[ - (2:(nk - 1)), ] # only need col 1:2
    ord <- dim(coeff)[2]
    if(ord >= 3) coeff[, 3:ord] <- 0
    deriv <- as.integer(deriv)
    while(deriv) {
	ord <- ord - 1
	## could be simplified when coeff has <= 2 non-zero cols:
	coeff <- t(t(coeff[, -1]) * (1:ord))# 1:ord = the 'k' in k* x^{k-1}
	deriv <- deriv - 1
    }
    nc <- ncol(coeff)
    if(any(which <- (x < knots[1]) & is.na(y)))
	y[which] <- if(nc==0) 0 else if(nc==1) coeff[1, 1]
	else coeff[1, 1] + coeff[1, 2] * (x[which] - knots[1])
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
	x <- seq.int(knots[1], knots[1] + period, length.out = nseg + 1)
    x.original <- x

    if(any(ind <- x < knots[1]))
	x[ind] <- x[ind] + period * (1 + (knots[1] - x[ind]) %/% period)
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
	args <- c(args, list(xlab = deparse(form[[3]]), ylab = deparse(form[[2]])))
    }
    args <- c(list(...), args)
###  do.call("xyplot", args)
    do.call("plot", args[unique(names(args))])
}

print.polySpline <- function(x, ...)
{
    coeff <- coef(x)
    dnames <- dimnames(coeff)
    if (is.null(dnames[[2]]))
	dimnames(coeff) <-
	    list(format(splineKnots(x)),
		 c("constant", "linear", "quadratic", "cubic",
		   paste(4:29, "th", sep = ""))[1:(dim(coeff)[2])])
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
    bknots <- coeff[, 1]
    adiff <- diff(bknots)
    if(!(all(adiff < 0) || all(adiff > 0)))
	stop("spline must be monotone")
    bcoeff <- array(0, dim(coeff))
    bcoeff[, 1] <- knots
    bcoeff[, 2] <- 1/coeff[, 2]
    a <- array(c(adiff^2, 2 * adiff, adiff^3, 3 * adiff^2),
	       c(nkm1, 2, 2))
    b <- array(c(kdiff - adiff * bcoeff[ - nk, 2],
		 bcoeff[-1, 2] - bcoeff[ - nk, 2]), c(nkm1, 2))
    for(i in 1:(nkm1))
	bcoeff[i, 3:4] <- solve(a[i,  ,	 ], b[i,  ])
    bcoeff[nk, 2:4] <- NA
    if(nk > 2) {
	bcoeff[1, 4] <- bcoeff[nkm1, 4] <- 0
	bcoeff[1, 2:3] <- solve(array(c(adiff[1], 1, adiff[1]^2,
					2 * adiff[1]), c(2, 2)),
				c(kdiff[1], 1/coeff[2, 2]))
	bcoeff[nkm1, 3] <- (kdiff[nkm1] - adiff[nkm1] *
			    bcoeff[nkm1, 2])/adiff[nkm1]^2
    }
    if(bcoeff[1, 3] > 0) {
	bcoeff[1, 3] <- 0
	bcoeff[1, 2] <- kdiff[1]/adiff[1]
    }
    if(bcoeff[nkm1, 3] < 0) {
	bcoeff[nkm1, 3] <- 0
	bcoeff[nkm1, 2] <- kdiff[nkm1]/adiff[nkm1]
    }
    value <- list(knots = bknots, coefficients = bcoeff)
    attr(value, "formula") <- do.call("~", as.list(attr(object, "formula"))[3:2])
    class(value) <- c("polySpline", "spline")
    value
}

backSpline.nbSpline <- function(object) backSpline(polySpline(object))
