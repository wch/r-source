#  File src/library/base/R/chol.R
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

chol <- function(x, ...) UseMethod("chol")

chol.default <- function(x, pivot = FALSE, LINPACK = FALSE, tol = -1, ...)
{
    if (is.complex(x))
        stop("complex matrices not permitted at present")

    if (!pivot && LINPACK) {
        warning("LINPACK = TRUE, pivot = FALSE is defunct and will be ignored",
                domain = NA)
        LINPACK <- FALSE
    }

    if(!LINPACK) return(.Internal(La_chol(as.matrix(x), pivot, tol)))

    ## deprecated in R 3.0.0
    warning("LINPACK = pivot = TRUE is deprecated", domain = NA)

    if(is.matrix(x)) {
	if(nrow(x) != ncol(x)) stop("non-square matrix in 'chol'")
	n <- nrow(x)
    } else {
	if(length(x) != 1L) stop("non-matrix argument to 'chol'")
	n <- 1L
    }

    ## sanity checks
    n <- as.integer(n)
    if(is.na(n)) stop("invalid nrow(x)")
    if(n > 46340) stop("too large a matrix for LINPACK")

    storage.mode(x) <- "double"

    xx <- x
    xx[lower.tri(xx)] <- 0
    z <- .Fortran(.F_dchdc, x = xx, n, n, double(n), piv = integer(n),
                  as.integer(pivot), rank = integer(1L), DUP = FALSE)
    if (z$rank < n)
        if(!pivot) stop("matrix not positive definite")
        else warning("matrix not positive definite")
    robj <- z$x
    if (pivot) {
        attr(robj, "pivot") <- z$piv
        attr(robj, "rank") <- z$rank
        if(!is.null(cn <- colnames(x)))
            colnames(robj) <- cn[z$piv]
    }
    robj
}

chol2inv <- function(x, size = NCOL(x), LINPACK = FALSE)
{
    if(LINPACK) warning("LINPACK = TRUE is defunct", domain = NA)
    .Internal(La_chol2inv(x, size))
}
