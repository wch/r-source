#  File src/library/stats/R/spline.R
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
####               --------- has more
####  also consider ``compatibility'' with  'approx' and 'approxfun'

spline <-
    function(x, y=NULL, n=3*length(x), method="fmm", xmin=min(x), xmax=max(x),
             xout, ties = mean)
{
    method <- pmatch(method, c("periodic", "natural", "fmm"))
    if(is.na(method))
	stop("invalid interpolation method")
	
    x <- regularize.values(x, y, ties) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- length(x)
	
    if(nx == 0) stop("zero non-NA points")
    if(method == 1 && y[1L] != y[nx]) { # periodic
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1L]
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
    if(missing(xout))
        xout <- seq.int(xmin, xmax, length.out=n)
    else n <- length(xout)
    if (n <= 0)
        stop("'spline' requires n >= 1")
    .C("spline_eval",
       z$method,
       nu=as.integer(n),
       x =as.double(xout),
       y =double(n),
       z$n,
       z$x,
       z$y,
       z$b,
       z$c,
       z$d,
       PACKAGE="stats")[c("x","y")]
}
