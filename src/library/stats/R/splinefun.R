#  File src/library/stats/R/splinefun.R
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

#### 'spline' and 'splinefun' are very similar --- keep in sync!
####  also consider ``compatibility'' with  'approx' and 'approxfun'

splinefun <- function(x, y=NULL, method = "fmm", ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    nx <- length(x)
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique 'x' values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	    rm(ux)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if(nx == 0) stop("zero non-NA points")
    if(method == 1 && y[1] != y[nx]) { # periodic
	warning("spline: first and last y values differ - using y[1] for both")
	y[nx] <- y[1]
    }
    z <- .C("spline_coef",
	    method=as.integer(method),
	    n=as.integer(nx),
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(method == 1) nx else 0),
	    PACKAGE="stats")
    rm(x,y,nx,o,method)
    z$e <- NULL
    function(x, deriv = 0) {
	deriv <- as.integer(deriv)
	if (deriv < 0 || deriv > 3)
	    stop("'deriv' must be between 0 and 3")
	if (deriv > 0) {
	    ## For deriv >= 2, using approx() should be faster, but doing it correctly
	    ## for all three methods is not worth the programmer's time...
	    z0 <- double(z$n)
	    z[c("y", "b", "c")] <-
		switch(deriv,
		       list(y=	 z$b, b = 2*z$c, c = 3*z$d), # deriv = 1
		       list(y= 2*z$c, b = 6*z$d, c =	z0), # deriv = 2
		       list(y= 6*z$d, b =    z0, c =	z0)) # deriv = 3
	    z[["d"]] <- z0
	}
	.C("spline_eval",
	   z$method,
	   as.integer(length(x)),
	   x=as.double(x),
	   y=double(length(x)),
	   z$n,
	   z$x,
	   z$y,
	   z$b,
	   z$c,
	   z$d,
	   PACKAGE="stats")$y
    }
}
