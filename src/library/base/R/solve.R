#  File src/library/base/R/solve.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

solve.qr <- function(a, b, ...)
{
    if(!inherits(a, "qr"))
	stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

solve.default <-
    function(a, b, tol = .Machine$double.eps, LINPACK = FALSE, ...)
{
    if (!missing(LINPACK))
        stop("the LINPACK argument has been defunct since R 3.1.0")

    if(inherits(a, "qr")) {
	warning("solve.default called with a \"qr\" object: use 'qr.solve'")
	return(solve.qr(a, b)) # used to pass tol, but that is unused
    }

    a <- as.matrix(a)
    isc <- is.complex(a)
    if(missing(b)) {
        b <- diag(if(isc) 1.0 + 0.0i else 1.0, nrow(a))
        colnames(b) <- rownames(a)
    }
    if(isc || (!missing(b) && is.complex(b)))
        .Internal(La_solve_cmplx(a, b, tol))
    else
        .Internal(La_solve(a, b, tol))
}

solve <- function(a, b, ...) UseMethod("solve")

qr.solve <- function(a, b, tol = 1e-7)
{
    if(!inherits(a, "qr"))
	a <- qr(a, tol = tol)
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
	stop("singular matrix 'a' in solve")
    if( missing(b) ) {
	if( nc != nr )
	    stop("only square matrices can be inverted")
	b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res
}

