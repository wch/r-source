#  File src/library/stats/R/bandwidths.R
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

### copyright (C) 1994-2001 W. N. Venables and B. D. Ripley
### This version distributed under GPL (version 2 or later)


#====           bandwidth selection rules              ====

bw.nrd0 <- function (x)
{
    if(length(x) < 2L) stop("need at least 2 data points")
    hi <- sd(x)
    if(!(lo <- min(hi, IQR(x)/1.34)))# qnorm(.75) - qnorm(.25) = 1.34898
        (lo <- hi) || (lo <- abs(x[1L])) || (lo <- 1.)
    0.9 * lo * length(x)^(-0.2)
}

bw.nrd <- function (x)
{
    if(length(x) < 2L) stop("need at least 2 data points")
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}

bw.SJ <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                  method = c("ste", "dpi"), tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")
    storage.mode(x) <- "double"
    method <- match.arg(method)

    fSD <- function(h, x, alph2, c1, n, d)
        (c1/SDh(x, alph2 * h^(5/7), n, d))^(1/5) - h
    SDh <- function(x, h, n, d)
        .C(C_band_phi4_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u
    TDh <- function(x, h, n, d)
        .C(C_band_phi6_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    Z <- .C(C_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    scale <- min(sd(x), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2*sqrt(pi)*n)
    TD  <- -TDh(cnt, b, n, d)
    if(!is.finite(TD) || TD <= 0)
        stop("sample is too sparse to find TD")
    if(method == "dpi")
        res <- (c1/SDh(cnt, (2.394/(n * TD))^(1/7), n, d))^(1/5)
    else {
        if(bnd.Miss <- missing(lower) || missing(upper)) {
            ## only used for  lower & upper  defaults :
            hmax <- 1.144 * scale * n^(-1/5)
            ##              ^^^^^ used to be  sd(x)  which maybe way off
        }
        alph2 <- 1.357*(SDh(cnt, a, n, d)/TD)^(1/7)
        if(!is.finite(alph2))
            stop("sample is too sparse to find alph2")
        itry <- 1
	while (fSD(lower, cnt, alph2, c1, n, d) *
	       fSD(upper, cnt, alph2, c1, n, d) > 0) {
	    if(itry > 99 || !bnd.Miss) # 1.2 ^ 99 = 69'014'979 .. enough
		stop("no solution in the specified range of bandwidths")
	    if(itry %% 2)
		upper <- upper * 1.2
	    else lower <- lower / 1.2
	    if(getOption("verbose"))
		message(gettextf("increasing bw.SJ() search interval (%d) to [%.4g,%.4g]",
			itry, lower, upper), domain = NA)
	    itry <- itry + 1
	}
        res <- uniroot(fSD, c(lower, upper), tol=tol,
                       x=cnt, alph2=alph2, c1=c1, n=n, d=d)$root
    }
    res
}


bw.ucv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fucv <- function(h, x, n, d)
        .C(C_band_ucv_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C(C_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fucv, c(lower, upper), tol=tol,
                  x=cnt, n=n, d=d)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}

bw.bcv <- function(x, nb = 1000, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    if(!is.numeric(x)) stop("invalid 'x'")

    fbcv <- function(h, x, n, d)
        .C(C_band_bcv_bin,
           as.integer(n),
           as.integer(length(x)),
           as.double(d),
           x,
           as.double(h),
           u = double(1))$u

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    storage.mode(x) <- "double"
    Z <- .C(C_band_den_bin,
            as.integer(n),
            as.integer(nb),
            d = double(1),
            x,
            cnt = integer(nb))
    d <- Z$d; cnt <- as.integer(Z$cnt)
    h <- optimize(fbcv, c(lower, upper), tol=tol,
                  x=cnt, n=n, d=d)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}
