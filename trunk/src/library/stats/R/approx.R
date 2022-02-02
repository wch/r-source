#  File src/library/stats/R/approx.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

### approx() and approxfun() are *very similar* -- keep in sync!

## This function is used in approx, approxfun, spline, and splinefun
## to massage the input (x,y) pairs into standard form:
## x values unique and increasing, y values collapsed to match
## (except if ties=="ordered", then not unique)
regularize.values <- function(x, y, ties, warn.collapsing = TRUE, na.rm = TRUE) {
    x <- xy.coords(x, y, setLab = FALSE) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    keptNA <- FALSE
    nx <-
    if(any(na <- is.na(x) | is.na(y))) {
	ok <- !na
      if(na.rm) {
	x <- x[ok]
	y <- y[ok]
        length(x)
      } else { ## na.rm is FALSE
          keptNA <- TRUE
          sum(ok)
      }
    } else {
        length(x)
    }
    if (!identical(ties, "ordered")) {
	ordered <-
	    if(is.function(ties) || is.character(ties))# fn or name of one
		FALSE
	    else if(is.list(ties) && length(T <- ties) == 2L && is.function(T[[2]])) {
		## e.g. ties ==  list("ordered", mean)
		ties <- T[[2]]
		identical(T[[1]], "ordered")
	    } else
		stop("'ties' is not \"ordered\", a function, or list(<string>, <function>)")
	if(!ordered && is.unsorted(if(keptNA) x[ok] else x)) {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
	if (length(ux <- unique(x)) < nx) {
	    if (warn.collapsing)
		warning("collapsing to unique 'x' values")
	    # tapply bases its uniqueness judgement on character representations;
	    # we want to use values (PR#14377)
	    y <- as.vector(tapply(y, match(x,x), ties))# as.v: drop dim & dimn.
	    x <- ux
	    stopifnot(length(y) == length(x))# (did happen in 2.9.0-2.11.x)
            if(keptNA) ok <- !is.na(x)
	}
    }
    list(x=x, y=y, keptNA=keptNA, notNA = if(keptNA) ok)
}

approx <- function(x, y = NULL, xout, method = "linear", n = 50,
		   yleft, yright, rule = 1, f = 0, ties = mean, na.rm = TRUE)
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method)) stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, lenR <= 2L)
    if(lenR == 1) rule <- rule[c(1,1)]
    r <- regularize.values(x, y, ties, missing(ties), na.rm=na.rm)
                                        # -> (x,y) numeric of same length
    y <- r$y
    x <- r$x
    noNA <- na.rm || !r$keptNA
    nx <- if(noNA)
              length(x) # large vectors ==> non-integer
          else
              sum(r$notNA)
    if (is.na(nx)) stop("invalid length(x)")
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
	if (n <= 0) stop("'approx' requires n >= 1")
        xout <-
            if(noNA)
                seq.int(x[1L], x[nx], length.out = n)
            else {
                xout <- x[r$notNA]
                seq.int(xout[1L], xout[length(xout)], length.out = n)
            }
    }
    x <- as.double(x); y <- as.double(y)
    .Call(C_ApproxTest, x, y, method, f, na.rm)
    yout <- .Call(C_Approx, x, y, xout, method, yleft, yright, f, na.rm)
    list(x = xout, y = yout)
}

approxfun <- function(x, y = NULL, method = "linear",
		   yleft, yright, rule = 1, f = 0, ties = mean, na.rm = TRUE)
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method)) stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, lenR <= 2L)
    if(lenR == 1) rule <- rule[c(1,1)]
    x <- regularize.values(x, y, ties, missing(ties), na.rm=na.rm)
                                        # -> (x,y) numeric of same length
    nx <- if(na.rm || !x$keptNA)
              length(x$x) # large vectors ==> non-integer
          else
              sum(x$notNA)
    if (is.na(nx)) stop("invalid length(x)")
    if (nx <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(nx == 0) stop("zero non-NA points")
    }
    y <- x$y
    if (missing(yleft))
	yleft <- if (rule[1L] == 1) NA else y[1L]
    if (missing(yright))
	yright <- if (rule[2L] == 1) NA else y[length(y)]
    stopifnot(length(yleft) == 1L, length(yright) == 1L, length(f) == 1L)
    rm(rule, ties, lenR, nx) # we do not need nx, but summary.stepfun did.

    ## 1. Test input consistency once
    x <- as.double(x$x); y <- as.double(y)
    .Call(C_ApproxTest, x, y, method, f, na.rm)

    ## 2. Create and return function that does not test input validity...
    function(v) .approxfun(x, y, v, method, yleft, yright, f, na.rm)
}

## avoid capturing internal calls
## default for 'na.rm': for old saved approxfun() {incl ecdf()} results
.approxfun <- function(x, y, v,  method, yleft, yright, f, na.rm=TRUE)
    .Call(C_Approx, x, y, v, method, yleft, yright, f, na.rm)
