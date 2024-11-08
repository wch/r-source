#  File src/library/base/R/kappa.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998-2024 The R Core Team
#  Copyright (C) 1998 B. D. Ripley
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
        if(!length(x))
            0
        else if(anyNA(x))
	    NA_real_
	else
	    svd(x, nu = 0L, nv = 0L)$d[1L]
	## *faster* at least on some platforms {but possibly less accurate}:
	##sqrt(eigen(crossprod(x), symmetric=TRUE, only.values=TRUE)$values[1L])
    }
    else if(is.numeric(x) || is.logical(x))
	.Internal(La_dlange(x, type))
    else if(is.complex(x))
	.Internal(La_zlange(x, type))
    else stop(gettextf("invalid '%s': type \"%s\"", "x", typeof(x)), domain=NA)
} ## and define it as implicitGeneric, so S4 methods are consistent

kappa <- function(z, ...) UseMethod("kappa")

## Note that  all 4 Lapack version now work in the following
rcond <- function(x, norm = c("O","I","1"), triangular = FALSE, uplo = "U", ...) {
    norm <- match.arg(norm)
    stopifnot(length(d <- dim(x)) == 2L)
    if(!all(d)) # 0-extent
        return(1 / 0)
    if(d[1L] != d[2L])## non-square matrix -- use QR
        return(rcond(qr.R(qr(if(d[1L] < d[2L]) t(x) else x)), triangular=TRUE, uplo="U",
                     norm=norm, ...))
    ## else,  x = square n x n matrix, n >= 1 :
    if(is.complex(x)) {
	if(triangular) switch(uplo,
			      "U" = .Internal(La_ztrcon (x, norm)),
			      "L" = .Internal(La_ztrcon3(x, norm, "L")),
			      stop("'uplo' must be \"U\" or \"L\""))
        else .Internal(La_zgecon(x, norm))
    } else {
	if(triangular) switch(uplo,
			      "U" = .Internal(La_dtrcon (x, norm)),
			      "L" = .Internal(La_dtrcon3(x, norm, "L")),
			      stop("'uplo' must be \"U\" or \"L\""))
        else .Internal(La_dgecon(x, norm))
    }
}

kappa.default <- function(z, exact = FALSE,
                          norm = NULL, method = c("qr", "direct"),
                          inv_z = solve(z),
                          triangular = FALSE, uplo = "U", ...)
{
    z <- as.matrix(z)
    if(!all(d <- dim(z))) # 0-extent matrix
        return(0)
    nNorm <- is.null(norm)
    if(exact) {
	if(nNorm || norm == "2") {
	    s <- svd(z, nu = 0L, nv = 0L)$d # decreasing, non-negative
	    if(s[1]) s[1]/s[length(s)] else Inf # when s is all zero
	}
	else {
	    if(nNorm) norm <- "1"
	    norm(z, type=norm) * norm(inv_z, type=norm)
	    ## was warning(.. "norm '%s' currently always uses exact = FALSE" ..)
	}
    }
    else if(match.arg(method) == "qr" || d[1L] != d[2L])
        kappa.qr(qr(if(d[1L] < d[2L]) t(z) else z),
                 exact = FALSE, norm = norm, ...)
    ## else {exact=FALSE, method = "direct", square matrix} :
    else if(triangular)
        .kappa_tri(z, exact = FALSE, norm = norm, uplo = uplo, ...)
    else if(is.complex(z))
        1/.Internal(La_zgecon(z, if(nNorm) "1" else norm))
    else
        1/.Internal(La_dgecon(z, if(nNorm) "1" else norm))
}

kappa.lm <- function(z, ...) kappa.qr(z$qr, ...)

kappa.qr <- function(z, ...)
{
    stopifnot(length(d <- dim(qr <- z$qr)) == 2L)
    R <- if(d[1L] > d[2L])
	     qr[seq_len(d[2L]), , drop = FALSE]
	 else if(d[1L] < d[2L])
	     qr[, seq_len(d[1L]), drop = FALSE]
	 else # square: n x n
	     qr
    R[lower.tri(R)] <- 0 # (for exact case)
    .kappa_tri(R, ...)
}

.kappa_tri <- function(z, exact = FALSE, LINPACK = TRUE, norm=NULL, uplo = "U", ...)
{  ##  ^^^ [tri]angular ==> use only triangular part, also for symmetric
    if(!all(dim(z))) # 0-extent matrix: norm(<empty>) = 0  ==> norm(z) * norm(z^{-1}) = 0 * 0 = 0 :
        return(0)
    if(exact) {
        if(is.null(norm) || identical("2", norm)) { # 2-norm : *not* assuming 'triangular'
            ## identically to kappa.default(z, exact=TRUE) :
            s <- svd(z, nu = 0L, nv = 0L)$d
            if(s[1]) s[1]/s[length(s)] else Inf
        }
        else norm(z, type=norm) * norm(solve(z), type=norm) # == kappa.default(z, exact=TRUE, norm=norm,..)
    }
    else {
        p <- as.integer(nrow(z))
        if(is.na(p)) stop("invalid nrow(z)")
	if(p != ncol(z)) stop("triangular matrix should be square")
	if(is.null(norm)) norm <- "1"
	else if(norm == "2") {
	    warning("using 1-norm approximation for approximate 2-norm")
	    norm <- "1"
	}
	else if(!match(toupper(norm), c("1", "O", "I"), 0L)) {
	    warning(gettextf("norm=\"%s\" not available here; using norm=\"1\"",
			     norm), domain = NA)
	    norm <- "1"
	}
	## norm is "1" ("O") or "I(nf)" :
	if(is.complex(z))
            1/.Internal(La_ztrcon3(z, norm, uplo))
	else if(LINPACK) {
	    if(norm == "I") # instead of "1" / "O"
		z <- t(z)
	    ##	dtrco  *differs* from Lapack's dtrcon() quite a bit
	    ## even though dtrco's doc also say to compute the
	    ## 1-norm reciprocal condition
            storage.mode(z) <- "double"
	    1 / .Fortran(.F_dtrco, z, p, p, k = double(1), double(p),
			 if(uplo == "U") 1L else 0L)$k
	}
	else 1/.Internal(La_dtrcon3(z, norm, uplo))
    }
}
