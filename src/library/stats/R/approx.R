#  File src/library/stats/R/approx.R
#  Part of the R package, http://www.R-project.org
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

### approx() and approxfun() are *very similar* -- keep in sync!

## This function is used in approx, approxfun, spline, and splinefun
## to massage the input (x,y) pairs into standard form:  
## x values unique and increasing, y values collapsed to match
## (except if ties=="ordered", then not unique)

regularize.values <- function(x, y, ties) {
    x <- xy.coords(x, y) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    if(any(na <- is.na(x) | is.na(y))) {
	ok <- !na
	x <- x[ok]
	y <- y[ok]
    }
    nx <- length(x)
    if (!identical(ties, "ordered")) {	    
    	o <- order(x)
	x <- x[o]
	y <- y[o]	
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    # tapply bases its uniqueness judgement on character representations;
	    # we want to use values (PR#14377)
	    y <- as.vector(tapply(y,match(x,x),ties))# as.v: drop dim & dimn.
	    x <- ux
	    stopifnot(length(y) == length(x))# (did happen in 2.9.0-2.11.x)
	}
    }
    list(x=x, y=y)
}

approx <- function(x, y = NULL, xout, method = "linear", n = 50,
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, lenR <= 2L)
    if(lenR == 1) rule <- rule[c(1,1)]    
    x <- regularize.values(x, y, ties) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- length(x)
    if (nx <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(nx == 0) stop("zero non-NA points")
    }

    if (missing(yleft))
	yleft <- if (rule[1L] == 1) NA else y[1L]
    if (missing(yright))
	yright <- if (rule[2L] == 1) NA else y[length(y)]
    stopifnot(length(yleft) == 1L, length(yright) == 1L, length(f) == 1L)
    if (missing(xout)) {
	if (n <= 0)
	    stop("'approx' requires n >= 1")
	xout <- seq.int(x[1L], x[nx], length.out = n)
    }
    y <- .C("R_approx", as.double(x), as.double(y), as.integer(nx),
	    xout = as.double(xout), as.integer(length(xout)),
	    as.integer(method), as.double(yleft), as.double(yright),
	    as.double(f), NAOK = TRUE, PACKAGE = "stats")$xout
    list(x = xout, y = y)
}

approxfun <- function(x, y = NULL, method = "linear",
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, lenR <= 2L)
    if(lenR == 1) rule <- rule[c(1,1)]
    x <- regularize.values(x, y, ties) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    n <- length(x)

    if (n <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(n == 0) stop("zero non-NA points")
    }
    if (missing(yleft))
	yleft <- if (rule[1L] == 1) NA else y[1L]
    if (missing(yright))
	yright <- if (rule[2L] == 1) NA else y[length(y)]
    force(f)
    stopifnot(length(yleft) == 1L, length(yright) == 1L, length(f) == 1L)
    rm(rule, ties, lenR)

## Changed here:
## suggestion:
    # 1. Test input consistency once
    .C("R_approxtest",as.double(x), as.double(y), as.integer(n),
        as.integer(method), as.double(f), NAOK = TRUE,
        PACKAGE = "stats")

    # 2. Create and return function that does not test input validity...
    function(v) .C("R_approxfun", as.double(x), as.double(y), as.integer(n),
        xout = as.double(v), as.integer(length(v)), as.integer(method),
        as.double(yleft), as.double(yright), as.double(f), NAOK = TRUE,
        PACKAGE = "stats")$xout
}

### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop("'vec' contains NAs")
    if(is.unsorted(vec))
	stop("'vec' must be sorted non-decreasingly")
    ## deal with NA's in x:
    if(has.na <- any(ix <- is.na(x)))
	x <- x[!ix]
    nx <- length(x)
    index <- integer(nx)
    .C("find_interv_vec",
       xt = as.double(vec), n = as.integer(length(vec)),
       x  = as.double(x),  nx = as.integer(nx),
       as.logical(rightmost.closed),
       as.logical(all.inside),
       index, DUP = FALSE, NAOK = TRUE, # NAOK: 'Inf' only
       PACKAGE = "base")
    if(has.na) {
	ii <- as.integer(ix)
	ii[ix] <- NA
	ii[!ix] <- index
	ii
    } else index
}
