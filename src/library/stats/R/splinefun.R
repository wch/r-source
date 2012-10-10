#  File src/library/stats/R/splinefun.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

splinefun <-
    function(x, y = NULL,
             method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"),
             ties = mean)
{
    x <- regularize.values(x, y, ties) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- as.integer(length(x))
    if(is.na(nx)) stop("invalid value of length(x)")
    if(nx == 0) stop("zero non-NA points")
    method <- match.arg(method)
    if(method == "periodic" && y[1L] != y[nx]) {
	warning("spline: first and last y values differ - using y[1L] for both")
	y[nx] <- y[1L]
    }
    if(method == "monoH.FC") {
        n1 <- nx - 1L
        ## - - - "Data preprocessing" - - -

        dy <- y[-1L] - y[-nx]           # = diff(y)
        dx <- x[-1L] - x[-nx]           # = diff(x)
        Sx <- dy / dx # Sx[k] =  \Delta_k = (y_{k+1} - y_k)/(x_{k+1} - x_k), k=1:n1
        m <- c(Sx[1L], (Sx[-1L] + Sx[-n1])/2, Sx[n1]) ## 1.

        ## use C, as we need to "serially" progress from left to right:
        m <- .Call(C_R_monoFC_m, m, Sx)

        ## Hermite spline with (x,y,m) :
        return(splinefunH0(x = x, y = y, m = m, dx = dx))
    }
    ## else
    iMeth <- match(method, c("periodic", "natural", "fmm",
                             "monoH.FC", "hyman"))
    if(iMeth == 5L) {
        dy <- diff(y)
        if(!(all(dy >= 0) || all(dy <= 0)))
            stop("'y' must be increasing or decreasing")
    }
    z <- .C(C_spline_coef,
	    method=as.integer(min(3L, iMeth)),
	    n=nx,
	    x=x,
	    y=y,
	    b=double(nx),
	    c=double(nx),
	    d=double(nx),
	    e=double(if(iMeth == 1) nx else 0))
    if(iMeth == 5L) z <- spl_coef_conv(hyman_filter(z))
    rm(x, y, nx, method, iMeth, ties)
    z$e <- NULL
    function(x, deriv = 0L) {
	deriv <- as.integer(deriv)
	if (deriv < 0L || deriv > 3L)
	    stop("'deriv' must be between 0 and 3")
	if (deriv > 0L) {
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
        ## yout[j] := y[i] + dx*(b[i] + dx*(c[i] + dx* d_i))
        ##           where dx := (u[j]-x[i]); i such that x[i] <= u[j] <= x[i+1},
        ##                u[j]:= xout[j] (unless sometimes for periodic spl.)
        ##           and  d_i := d[i] unless for natural splines at left
	res <- .C(C_spline_eval,
                  z$method,
                  as.integer(length(x)),
                  x=as.double(x),
                  y=double(length(x)),
                  z$n,
                  z$x,
                  z$y,
                  z$b,
                  z$c,
                  z$d)$y

        ## deal with points to the left of first knot if natural
        ## splines are used  (Bug PR#13132)
        if( deriv > 0 && z$method==2 && any(ind <- x<=z$x[1L]) )
          res[ind] <- ifelse(deriv == 1, z$y[1L], 0)

        res
    }
}

## hidden : The exported user function is splinefunH()
splinefunH0 <- function(x, y, m, dx = x[-1L] - x[-length(x)])
{
    function(u, deriv=0, extrapol = c("linear","cubic"))
    {
	extrapol <- match.arg(extrapol)
	deriv <- as.integer(deriv)
	if (deriv < 0 || deriv > 2)
	    stop("'deriv' must be between 0 and 2")
	i <- findInterval(u, x, all.inside = (extrapol == "cubic"))
	if(deriv == 0)
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## Compute the 4 Hermite (cubic) polynomials h00, h01,h10, h11
		t1 <- t-1
		h01 <- t*t*(3 - 2*t)
		h00 <- 1 - h01
		tt1 <- t*t1
		h10 <- tt1 * t1
		h11 <- tt1 * t
		y[i]  * h00 + h*m[i]  * h10 +
		y[i+1]* h01 + h*m[i+1]* h11
	    }
	else if(deriv == 1)
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## 1st derivative of Hermite polynomials h00, h01,h10, h11
		t1 <- t-1
		h01 <- -6*t*t1 # h00 = - h01
		h10 <- (3*t - 1) * t1
		h11 <- (3*t - 2) * t
		(y[i+1] - y[i])/h * h01 + m[i] * h10 + m[i+1]* h11
	    }
	else ## deriv == 2
	    interp <- function(u, i) {
		h <- dx[i]
		t <- (u - x[i]) / h
		## 2nd derivative of Hermite polynomials h00, h01,h10, h11
		h01 <- 6*(1-2*t) # h00 = - h01
		h10 <- 2*(3*t - 2)
		h11 <- 2*(3*t - 1)
		((y[i+1] - y[i])/h * h01 + m[i] * h10 + m[i+1]* h11) / h
	    }


	if(extrapol == "linear" &&
	   any(iXtra <- (iL <- (i == 0)) | (iR <- (i == (n <- length(x)))))) {
	    ##	do linear extrapolation
	    r <- u
	    if(any(iL)) r[iL] <- if(deriv == 0) y[1L] + m[1L]*(u[iL] - x[1L]) else
				  if(deriv == 1) m[1L] else 0
	    if(any(iR)) r[iR] <- if(deriv == 0) y[n] + m[n]*(u[iR] - x[n]) else
				  if(deriv == 1) m[n] else 0
	    ## For internal values, compute "as normal":
	    ini <- !iXtra
	    r[ini] <- interp(u[ini], i[ini])
	    r
	}
        else { ## use cubic Hermite polynomials, even for extrapolation
            interp(u, i)
        }

    }
}

splinefunH <- function(x, y, m)
{
    ## Purpose: "Cubic Hermite Spline"
    ## ----------------------------------------------------------------------
    ## Arguments: (x,y): points;  m: slope at points,  all of equal length
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  9 Jan 2008
    n <- length(x)
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(m),
              length(y) == n, length(m) == n)
    if(is.unsorted(x)) {
        i <- sort.list(x)
        x <- x[i]
        y <- y[i]
        m <- m[i]
    }
    dx <- x[-1L] - x[-n]
    if(any(is.na(dx)) || any(dx == 0))
        stop("'x' must be *strictly* increasing (non - NA)")
    splinefunH0(x, y, m, dx=dx)
}
