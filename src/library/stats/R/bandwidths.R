#  File src/library/stats/R/bandwidths.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1994-2001 W. N. Venables and B. D. Ripley
#  Copyright (C) 2001-2012 The R Core Team
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

bw.SJ <- function(x, nb = 1000L, lower = 0.1*hmax, upper = hmax,
                  method = c("ste", "dpi"), tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) stop("invalid length(x)")
    if(!is.numeric(x)) stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) stop("invalid 'nb'")
    storage.mode(x) <- "double"

    method <- match.arg(method)

    fSD <- function(h) ( c1 / SDh(alph2 * h^(5/7)) )^(1/5) - h
    SDh <- function(h) .Call(C_bw_phi4, n, d, cnt, as.double(h))
    TDh <- function(h) .Call(C_bw_phi6, n, d, cnt, as.double(h))

    Z <- .Call(C_bw_den, nb, x)
    d <- Z[[1L]]; cnt <- Z[[2L]]
    scale <- min(sd(x), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2*sqrt(pi)*n)
    TD  <- -TDh(b)
    if(!is.finite(TD) || TD <= 0)
        stop("sample is too sparse to find TD", domain = NA)
    if(method == "dpi")
        res <- (c1/SDh((2.394/(n * TD))^(1/7)))^(1/5)
    else {
        if(bnd.Miss <- missing(lower) || missing(upper)) {
            ## only used for  lower & upper  defaults :
            hmax <- 1.144 * scale * n^(-1/5)
        }
        alph2 <- 1.357*(SDh(a)/TD)^(1/7)
        if(!is.finite(alph2))
            stop("sample is too sparse to find alph2", domain  = NA)
        itry <- 1L
	while (fSD(lower) * fSD(upper) > 0) {
	    if(itry > 99L || !bnd.Miss) # 1.2 ^ 99 = 69'014'979 .. enough
		stop("no solution in the specified range of bandwidths")
	    if(itry %% 2) upper <- upper * 1.2 else lower <- lower / 1.2
	    if(getOption("verbose"))
		message(gettextf("increasing bw.SJ() search interval (%d) to [%.4g,%.4g]",
                                 itry, lower, upper), domain = NA)
	    itry <- itry + 1L
	}
        res <- uniroot(fSD, c(lower, upper), tol = tol)$root
    }
    res
}


bw.ucv <- function(x, nb = 1000L, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) stop("invalid length(x)")
    if(!is.numeric(x)) stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) stop("invalid 'nb'")
    storage.mode(x) <- "double"

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    Z <- .Call(C_bw_den, nb, x); d <- Z[[1L]]; cnt <- Z[[2L]]
    fucv <- function(h) .Call(C_bw_ucv, n, d, cnt, as.double(h))
    h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}

bw.bcv <- function(x, nb = 1000L, lower = 0.1*hmax, upper = hmax,
                   tol = 0.1*lower)
{
    if((n <- length(x)) < 2L) stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) stop("invalid length(x)")
    if(!is.numeric(x)) stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) stop("invalid 'nb'")
    storage.mode(x) <- "double"

    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    Z <- .Call(C_bw_den, nb, x); d <- Z[[1L]]; cnt <- Z[[2L]]
    fbcv <- function(h) .Call(C_bw_bcv, n, d, cnt, as.double(h))
    h <- optimize(fbcv, c(lower, upper), tol = tol)$minimum
    if(h < lower+tol | h > upper-tol)
        warning("minimum occurred at one end of the range")
    h
}
