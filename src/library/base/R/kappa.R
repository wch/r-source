#  File src/library/base/R/kappa.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 1998-2012 The R Core Team
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

norm <- function(x, type = c("O", "I", "F", "M", "2")) {
    if(identical("2", type)) {
	svd(x, nu = 0L, nv = 0L)$d[1L]
	## *faster* at least on some platforms {but possibly less accurate}:
	##sqrt(eigen(crossprod(x), symmetric=TRUE, only.values=TRUE)$values[1L])
    } else
	.Internal(La_dlange(x, type))
} ## and define it as implicitGeneric, so S4 methods are consistent

kappa <- function(z, ...) UseMethod("kappa")

## Note that  all 4 Lapack version now work in the following
rcond <- function(x, norm = c("O","I","1"), triangular = FALSE, ...) {
    norm <- match.arg(norm)
    stopifnot(is.matrix(x))
    if({d <- dim(x); d[1L] != d[2L]})## non-square matrix -- use QR
        return(rcond(qr.R(qr(if(d[1L] < d[2L]) t(x) else x)), norm=norm, ...))

    ## x = square matrix :
    if(is.complex(x)) {
        if(triangular) .Internal(La_ztrcon(x, norm))
        else .Internal(La_zgecon(x, norm))
    } else {
        if(triangular) .Internal(La_dtrcon(x, norm))
        else .Internal(La_dgecon(x, norm))
    }
}

kappa.default <- function(z, exact = FALSE,
                          norm = NULL, method = c("qr", "direct"), ...)
{
    method <- match.arg(method)
    z <- as.matrix(z)
    norm <- if(!is.null(norm)) match.arg(norm, c("2", "1","O", "I")) else "2"
    if(exact && norm == "2") {
        s <- svd(z, nu = 0, nv = 0)$d
        max(s)/min(s[s > 0])
    }
    else { ## exact = FALSE or norm in "1", "O", "I"
	if(exact)
	    warning(gettextf("norm '%s' currently always uses exact = FALSE",
			     norm))
        d <- dim(z)
        if(method == "qr" || d[1L] != d[2L])
	    kappa.qr(qr(if(d[1L] < d[2L]) t(z) else z),
		     exact = FALSE, norm = norm, ...)
        else .kappa_tri(z, exact = FALSE, norm = norm, ...)
    }
}

kappa.lm <- function(z, ...) kappa.qr(z$qr, ...)

kappa.qr <- function(z, ...)
{
    qr <- z$qr
    R <- qr[1L:min(dim(qr)), , drop = FALSE]
    R[lower.tri(R)] <- 0
    .kappa_tri(R, ...)
}

.kappa_tri <- function(z, exact = FALSE, LINPACK = TRUE, norm=NULL, ...)
{
    if(exact) {
        stopifnot(is.null(norm) || identical("2", norm))
        kappa.default(z, exact = TRUE) ## using "2 - norm" !
    }
    else { ## norm is "1" ("O") or "I(nf)" :
        p <- as.integer(nrow(z))
        if(is.na(p)) stop("invalid nrow(x)")
	if(p != ncol(z)) stop("triangular matrix should be square")
	if(is.null(norm)) norm <- "1"
	if(is.complex(z)) 1/.Internal(La_ztrcon(z, norm))
	else if(LINPACK) {
	    if(norm == "I") # instead of "1" / "O"
		z <- t(z)
	    ##	dtrco  *differs* from Lapack's dtrcon() quite a bit
	    ## even though dtrco's doc also say to compute the
	    ## 1-norm reciprocal condition
            storage.mode(z) <- "double"
	    1 / .Fortran(.F_dtrco, z, p, p, k = double(1), double(p), 1L)$k
	}
	else 1/.Internal(La_dtrcon(z, norm))
    }
}
