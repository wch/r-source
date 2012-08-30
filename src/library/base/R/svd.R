#  File src/library/base/R/svd.R
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

svd <- function(x, nu = min(n,p), nv = min(n,p), LINPACK = FALSE)
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    dx <- dim(x)
    n <- dx[1L]
    p <- dx[2L]
    if(!n || !p) stop("0 extent dimensions")
    if (is.complex(x)) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) Conj(t(res$vt))))
    }
    if(!is.double(x))
	storage.mode(x) <- "double"
    if (!LINPACK) {
        res <- La.svd(x, nu, nv)
        return(list(d = res$d, u = if(nu) res$u, v = if(nv) t(res$vt)))
    }

    ## LINPACK only from here on.
    warning("LINPACK = TRUE is deprecated", domain = NA)
    n <- as.integer(n)
    if(is.na(n)) stop("invalid nrow(x)")
    p <- as.integer(p)
    if(is.na(p)) stop("invalid ncol(x)")

    if(nu == 0L) {
	job <- 0L
	u <- double()
    }
    else if(nu == n) {
	job <- 10L
	u <- matrix(0, n, n)
    }
    else if(nu == p) {
	job <- 20L
	u <- matrix(0, n, p)
    }
    else
	stop("'nu' must be 0, nrow(x) or ncol(x)")

    job <- job +
	if(nv == 0L) 0L else if(nv == p || nv == n) 1L else
    stop("'nv' must be 0 or ncol(x)")

    v <- if(job == 0L) double() else matrix(0, p, p)

    mm <- min(n+1L,p)
    z <- .Fortran("dsvdc",
		  x, # did storage.mode above
		  n,
		  n,
		  p,
		  d = double(mm),
		  double(p),
		  u = u,
		  n,
		  v = v,
		  p,
		  double(n),
		  as.integer(job),
		  info = integer(1L),
		  DUP = FALSE, PACKAGE = "base")[c("d","u","v","info")]
    if(z$info)
	stop(gettextf("error %d in 'dsvdc'", z$info), domain = NA)
    z$d <- z$d[seq_len(min(n, p))]
    if(nv && nv < p) z$v <- z$v[, 1L:nv, drop = FALSE]
    z[c("d", if(nu) "u", if(nv) "v")]
}
