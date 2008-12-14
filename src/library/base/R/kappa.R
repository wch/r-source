#  File src/library/base/R/kappa.R
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

kappa <- function(z, ...) UseMethod("kappa")

## Note that  all 4 Lapack version now work in the following
rcond <- function(x, norm = c("O","I","1"), triangular = FALSE, ...) {
    norm <- match.arg(norm)
    stopifnot(is.matrix(x))
    if({d <- dim(x); d[1L] != d[2L]})## non-square matrix -- use QR
        return(rcond(qr.R(qr(if(d[1L] < d[2L]) t(x) else x)), norm=norm, ...))

    ## x = square matrix :
    if(is.complex(x)) {
        if(triangular)
            .Call("La_ztrcon", x, norm, PACKAGE="base")
        else .Call("La_zgecon", x, norm, PACKAGE="base")
    }
    else {
        storage.mode(x) <- "double"
        if(triangular)
            .Call("La_dtrcon", x, norm, PACKAGE="base")
        else .Call("La_dgecon", x, norm, PACKAGE="base")
    }
}

kappa.default <- function(z, exact = FALSE,
                          norm = NULL, method = c("qr", "direct"), ...)
{
    method <- match.arg(method)
    z <- as.matrix(z)
    norm <- if(!is.null(norm)) match.arg(norm, c("2", "1","O", "I")) else "2"
    if(exact && norm == "2") {
        s <- svd(z, nu=0, nv=0)$d
        max(s)/min(s[s > 0])
    }
    else { ## exact = FALSE or norm in "1", "O", "I"
        d <- dim(z)
        if(method == "qr" || d[1L] != d[2L])
            kappa.qr(qr(if(d[1L] < d[2L]) t(z) else z), norm=norm, ...)
        else kappa.tri(z, exact=FALSE, norm=norm, ...)
    }
}

kappa.lm <- function(z, ...) kappa.qr(z$qr, ...)

kappa.qr <- function(z, ...)
{
    qr <- z$qr
    R <- qr[1L:min(dim(qr)), , drop = FALSE]
    R[lower.tri(R)] <- 0
    kappa.tri(R, ...)
}

kappa.tri <- function(z, exact = FALSE, LINPACK = TRUE, norm=NULL, ...)
{
    if(exact) {
        stopifnot(is.null(norm) || identical("2", norm))
        kappa.default(z, exact = TRUE) ## using "2 - norm" !
    }
    else {
	p <- nrow(z)
	if(p != ncol(z)) stop("triangular matrix should be square")
	if(is.null(norm)) norm <- "1"
	if(is.complex(z))
	    1/.Call("La_ztrcon", z, norm, PACKAGE="base")
	else if(LINPACK) {
	    if(norm == "I") # instead of "1" / "O"
		z <- t(z)
	    ##	dtrco  *differs* from Lapack's dtrcon() quite a bit
	    ## even though dtrco's doc also say to compute the
	    ## 1-norm reciprocal condition
	    1 / .Fortran("dtrco",
			 as.double(z),
			 p,
			 p,
			 k = double(1),
			 double(p),
			 1L,
			 PACKAGE="base")$k
	}
	else { ## Lapack
	    storage.mode(z) <- "double"
	    1/.Call("La_dtrcon", z, norm, PACKAGE="base")
	}
    }
}
