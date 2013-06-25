#  File src/library/base/R/svd.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

svd <- function(x, nu = min(n,p), nv = min(n,p), LINPACK = FALSE)
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    dx <- dim(x)
    n <- dx[1L]
    p <- dx[2L]
    if(!n || !p) stop("a dimension is zero")
    La.res <- La.svd(x, nu, nv)
    res <- list(d = La.res$d)
    if (nu) res$u <- La.res$u
    if (nv) {
	if (is.complex(x))
	    res$v <- Conj(t(La.res$vt))
	else
	    res$v <- t(La.res$vt)
    }
    res
}
