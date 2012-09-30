#  File src/library/base/R/LAPACK.R
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

La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.logical(x) && !is.numeric(x) && !is.complex(x))
	stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("a dimension is zero")

    if(is.complex(x)) {
        if(nu == 0L) {
            jobu <- "N"
            u <- matrix(0+0i, 1L, 1L)  # dim is checked
        }
        else if(nu == n) {
            jobu <- ifelse(n > p, "A", "S")
            u <- matrix(0+0i, n, n)
        }
        else if(nu == p) {
            jobu <- ifelse(n > p, "S", "A")
            u <- matrix(0+0i, n, p)
        }
        else
            stop("'nu' must be 0, nrow(x) or ncol(x)")

        if (nv == 0L) {
            jobv <- "N"
            v <- matrix(0+0i, 1L, 1L) # dim is checked
        }
        else if (nv == n) {
            jobv <- ifelse(n > p, "A", "S")
            v <- matrix(0+0i, min(n, p), p)
        }
        else if (nv == p) {
            jobv <- ifelse(n > p, "S", "A")
            v <- matrix(0+0i, p, p)
        }
        else
            stop("'nv' must be 0, nrow(x) or ncol(x)")
        res <- .Internal(La_svd_cmplx(jobu, jobv, x, double(min(n, p)), u, v))
        return(res[c("d", if(nu) "u", if(nv) "vt")])
    } else {
        if(nu || nv) {
            np <- min(n, p)
            if(nu <= np && nv <= np) {
                jobu <- "S"
                u <- matrix(0, n, np)
                vt <- matrix(0, np, p)
                nu0 <- nv0 <- np
            } else {
                jobu <- "A"
                u <- matrix(0, n, n)
                vt <- matrix(0, p, p)
                nu0 <- n; nv0 <- p
            }
        } else {
            jobu <- "N"
            # these dimensions _are_ checked, but unused
            u <- matrix(0, 1L, 1L)
            vt <- matrix(0, 1L, 1L)
        }
        ## type is now coerced internally
        res <- .Internal(La_svd(jobu, x, double(min(n,p)), u, vt))
        res <- res[c("d", if(nu) "u", if(nv) "vt")]
        if(nu && nu < nu0) res$u <- res$u[, 1L:min(n, nu), drop = FALSE]
        if(nv && nv < nv0) res$vt <- res$vt[1L:min(p, nv), , drop = FALSE]
        return(res)
    }
    ## not reached
}
