#  File src/library/stats/R/ARMAtheory.R
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

ARMAacf <- function(ar = numeric(), ma = numeric(), lag.max = r,
                    pacf = FALSE)
{
    p <- length(ar)
    q <- length(ma)
    if(!p && !q) stop("empty model supplied")
    r <- max(p, q + 1)
    if(p > 0) {
        if(r > 1) {
            if(r > p) { ## pad with zeros so p >= q+1
                ar <- c(ar, rep(0, r - p))
                p <- r
            }
            A <- matrix(0, p + 1L, 2L * p + 1L)
            ind <- as.matrix(expand.grid(1L:(p + 1), 1L:(p+1)))[, 2L:1L]
            ind[, 2] <- ind[, 1L] + ind[, 2L] - 1L
            A[ind] <- c(1, -ar)
            A[,  1L:p] <- A[, 1L:p] + A[, (2L * p + 1L):(p + 2L)]
            rhs <- c(1, rep(0, p))
            if(q > 0) {
                psi <- c(1, ARMAtoMA(ar, ma, q))
                theta <- c(1, ma, rep(0, q+1L))
                for(k in 1 + 0:q) rhs[k] <- sum(psi * theta[k + 0:q])
            }
            ind <- (p+1):1
            Acf <- solve(A[ind, ind], rhs)
	    Acf <- Acf[-1L]/Acf[1L]
        } else Acf <- ar
        if(lag.max > p) {
            xx <- rep(0, lag.max - p)
            Acf <- c(Acf, filter(xx, ar, "recursive", init = rev(Acf)))
        }
        Acf <- c(1, Acf[1L:lag.max])
    } else if(q > 0) {
        x <- c(1, ma)
        Acf <- filter(c(x, rep(0, q)), rev(x), sides=1)[-(1L:q)]
        if(lag.max > q) Acf <- c(Acf, rep(0, lag.max - q))
        Acf <- Acf/Acf[1L]
    }
    names(Acf) <- 0:lag.max
    if(pacf) drop(.Call(C_pacf1, Acf, lag.max)) else Acf
}

acf2AR <- function(acf)
{
    r <- as.double(drop(acf))
    order.max <- length(r) - 1
    if(order.max <= 0) stop("'acf' must be of length two or more")
    z <- .Fortran(C_eureka, as.integer(order.max), r, r,
                  coefs = double(order.max^2), vars = double(order.max),
                  double(order.max))
    nm <- paste0("ar(",1L:order.max, ")")
    matrix(z$coefs, order.max, order.max, dimnames=list(nm, 1L:order.max))
}

ARMAtoMA <- function(ar = numeric(), ma = numeric(), lag.max)
    .Call(C_ARMAtoMA, as.double(ar), as.double(ma), as.integer(lag.max))
