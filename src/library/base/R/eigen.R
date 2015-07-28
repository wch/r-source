#  File src/library/base/R/eigen.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


isSymmetric <- function(object, ...) UseMethod("isSymmetric")

isSymmetric.matrix <- function(object, tol = 100*.Machine$double.eps, ...)
{
    if(!is.matrix(object)) return(FALSE) ## we test for  symmetric *matrix*
    ## cheap pretest: is it square?
    d <- dim(object)
    if(d[1L] != d[2L]) return(FALSE)
    test <-
        if(is.complex(object))
            all.equal.numeric(object, Conj(t(object)), tolerance = tol, ...)
        else # numeric, character, ..
            all.equal(object, t(object), tolerance = tol, ...)
    isTRUE(test)
}

eigen <- function(x, symmetric, only.values = FALSE, EISPACK = FALSE)
{
    x <- unname(as.matrix(x))
    n <- nrow(x)
    if (!n) stop("0 x 0 matrix")
    if (n != ncol(x)) stop("non-square matrix in 'eigen'")
    n <- as.integer(n)
    if(is.na(n)) stop("invalid nrow(x)")

    complex.x <- is.complex(x)
    if (!all(is.finite(x))) stop("infinite or missing values in 'x'")

    if(missing(symmetric)) symmetric <- isSymmetric.matrix(x)

    if (symmetric) {
        z <- if(!complex.x) .Internal(La_rs(x, only.values))
        else .Internal(La_rs_cmplx(x, only.values))
        ord <- rev(seq_along(z$values))
    } else {
        z <- if(!complex.x) .Internal(La_rg(x, only.values))
        else .Internal(La_rg_cmplx(x, only.values))
        ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    return(list(values = z$values[ord],
                vectors = if (!only.values) z$vectors[, ord, drop = FALSE]))
}
