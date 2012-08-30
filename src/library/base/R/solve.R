#  File src/library/base/R/solve.R
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

solve.qr <- function(a, b, ...)
{
    if( !is.qr(a) )
	stop("this is the \"qr\" method for the generic function solve()")
    nc <- ncol(a$qr); nr <- nrow(a$qr)
    if( a$rank != min(nc, nr) )
    if( a$rank != nc )
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
    function(a, b, tol = ifelse(LINPACK, 1e-7, .Machine$double.eps),
	     LINPACK = FALSE, ...)
{
    if(is.complex(a) || (!missing(b) && is.complex(b))) {
	a <- as.matrix(a)
	if(missing(b)) {
            if(nrow(a) != ncol(a))
                stop("only square matrices can be inverted")
	    b <- diag(1.0+0.0i, nrow(a))
	    colnames(b) <- rownames(a)
	} else if(!is.complex(b)) b[] <- as.complex(b)
	if(!is.complex(a)) a[] <- as.complex(a)
	return (if (is.matrix(b)) {
            if(ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
	    rownames(b) <- colnames(a)
	    .Call("La_zgesv", a, b, PACKAGE = "base")
	} else
	    drop(.Call("La_zgesv", a, as.matrix(b), PACKAGE = "base")))
    }
    if(is.qr(a)) {
	warning("solve.default called with a \"qr\" object: use 'qr.solve'")
	return(solve.qr(a, b, tol))
    }

    if(!LINPACK) {
	a <- as.matrix(a)
	if(missing(b)) {
            if(nrow(a) != ncol(a))
                stop("only square matrices can be inverted")
	    b <- diag(1.0, nrow(a))
	    colnames(b) <- rownames(a)
	} else storage.mode(b) <- "double"
	storage.mode(a) <- "double"
	return (if (is.matrix(b)) {
            if(ncol(a) != nrow(b)) stop("'b' must be compatible with 'a'")
	    rownames(b) <- colnames(a)
	    .Call("La_dgesv", a, b, tol, PACKAGE = "base")
	} else
	    drop(.Call("La_dgesv", a, as.matrix(b), tol, PACKAGE = "base")))
    }
    warning("LINPACK = TRUE is deprecated", domain = NA)
    a <- qr(a, tol = tol)
    nc <- ncol(a$qr)
    if( a$rank != nc )
	stop("singular matrix 'a' in 'solve'")
    if( missing(b) ) {
	if( nc != nrow(a$qr) )
	    stop("only square matrices can be inverted")
	## preserve dimnames
	b <- diag(1, nc)
	colnames(b) <- rownames(a$qr)
    }
    qr.coef(a, b)
}

solve <- function(a, b, ...) UseMethod("solve")

qr.solve <- function(a, b, tol = 1e-7)
{
    if( !is.qr(a) )
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

