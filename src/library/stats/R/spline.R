#  File src/library/stats/R/spline.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#                2002 Simon N. Wood
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

#### 'spline' and 'splinefun' are very similar --- keep in sync!
####               --------- has more
####  also consider ``compatibility'' with  'approx' and 'approxfun'

spline <-
    function(x, y = NULL, n = 3*length(x), method = "fmm",
             xmin = min(x), xmax = max(x), xout, ties = mean)
{
    method <- pmatch(method, c("periodic", "natural", "fmm", "hyman"))
    if(is.na(method)) stop("invalid interpolation method")

    x <- regularize.values(x, y, ties) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- as.integer(length(x))
    if(is.na(nx)) stop("invalid value of length(x)")

    if(nx == 0) stop("zero non-NA points")

    if(method == 1L && y[1L] != y[nx]) { # periodic
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1L]
    }
    if(method == 4L) {
        dy <- diff(y)
        if(!(all(dy >= 0) || all(dy <= 0)))
            stop("'y' must be increasing or decreasing")
    }

    if(missing(xout)) xout <- seq.int(xmin, xmax, length.out = n)
    else n <- length(xout)
    if (n <= 0L) stop("'spline' requires n >= 1")
    xout <- as.double(xout)

    z <- .Call(C_SplineCoef, min(3L, method), x, y)
    if(method == 4L) z <- spl_coef_conv(hyman_filter(z))
    list(x = xout, y = .Call(C_SplineEval, xout, z))
}

### Filters cubic spline function to yield co-monotonicity in accordance
### with Hyman (1983) SIAM J. Sci. Stat. Comput. 4(4):645-654, z$x is knot
### position z$y is value at knot z$b is gradient at knot. See also
### Dougherty, Edelman and Hyman 1989 Mathematics of Computation 52:471-494.
### Contributed by Simon N. Wood, improved by R-core.
### https://stat.ethz.ch/pipermail/r-help/2002-September/024890.html
hyman_filter <- function(z)
{
    n <- length(z$x)
    ss <- diff(z$y) / diff(z$x)
    S0 <- c(ss[1L], ss)
    S1 <- c(ss, ss[n-1L])
    t1 <- pmin(abs(S0), abs(S1))
    sig <- z$b
    ind <- S0*S1 > 0
    sig[ind] <- S1[ind]
    ind <- sig >= 0
    if(sum(ind)) z$b[ind] <- pmin(pmax(0, z$b[ind]), 3*t1[ind])
    ind <- !ind
    if(sum(ind)) z$b[ind] <- pmax(pmin(0, z$b[ind]), -3*t1[ind])
    z
}


### Takes an object z containing equal-length vectors
### z$x, z$y, z$b, z$c, z$d defining a cubic spline interpolating
### z$x, z$y and forces z$c and z$d to be consistent with z$y and
### z$b (gradient of spline). This is intended for use in conjunction
### with Hyman's monotonicity filter.
### Note that R's spline routine has s''(x)/2 as c and s'''(x)/6 as d.
### Contributed by Simon N. Wood, improved by R-core.
spl_coef_conv <- function(z)
{
    n <- length(z$x)
    h <- diff(z$x); y <- -diff(z$y)
    b0 <- z$b[-n]; b1 <- z$b[-1L]
    cc <- -(3*y + (2*b0 + b1)*h) / h^2
    c1 <- (3*y[n-1L] + (b0[n-1L] + 2*b1[n-1L])*h[n-1L]) / h[n-1L]^2
    z$c <- c(cc, c1)
    dd <- (2*y/h + b0 + b1) / h^2
    z$d <- c(dd, dd[n-1L])
    z
}
