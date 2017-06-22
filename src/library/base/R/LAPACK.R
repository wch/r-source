#  File src/library/base/R/LAPACK.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.logical(x) && !is.numeric(x) && !is.complex(x))
	stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("a dimension is zero")
    zero <- if(is.complex(x)) 0+0i else 0

    if(nu || nv) {
        np <- min(n, p)
        if(nu <= np && nv <= np) {
            jobu <- "S"
            u <- matrix(zero, n, np)
            vt <- matrix(zero, np, p)
            nu0 <- nv0 <- np
        } else {
            jobu <- "A"
            u <- matrix(zero, n, n)
            vt <- matrix(zero, p, p)
            nu0 <- n; nv0 <- p
        }
    } else {
        jobu <- "N"
        ## these dimensions _are_ checked, but unused
        u <- matrix(zero, 1L, 1L)
        vt <- matrix(zero, 1L, 1L)
    }

    res <- if(is.complex(x))
       .Internal(La_svd_cmplx(jobu, x, double(min(n,p)), u, vt))
    else
       .Internal(La_svd(jobu, x, double(min(n,p)), u, vt))
    res <- res[c("d", if(nu) "u", if(nv) "vt")]
    if(nu && nu < nu0) res$u <- res$u[, seq_len(min(n, nu)), drop = FALSE]
    if(nv && nv < nv0) res$vt <- res$vt[seq_len(min(p, nv)), , drop = FALSE]
    res
}

La_version <- function() .Internal(La_version())
La_library <- function() .Internal(La_library())

