#  File src/library/base/R/outer.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

outer <- function (X, Y, FUN = "*", ...)
{
    if(is.array(X)) {
        dX <- dim(X)
        nx <- dimnames(X)
        no.nx <- is.null(nx)
    } else { # a vector
        dX <- length(X)  # cannot be long, as form a matrix below
        no.nx <- is.null(names(X))
        if(!no.nx) nx <- list(names(X))
    }
    if(is.array(Y)) {
        dY <- dim(Y)
        ny <- dimnames(Y)
        no.ny <- is.null(ny)
    } else { # a vector
        dY <- length(Y)
        no.ny <- is.null(names(Y))
        if(!no.ny) ny <- list(names(Y))
    }
    if (is.character(FUN) && FUN=="*") {
        if(!missing(...)) stop('using ... with FUN = "*" is an error')
        # this is for numeric vectors, so dropping attributes is OK
        robj <- as.vector(X) %*% t(as.vector(Y))
        dim(robj) <- c(dX, dY)
    } else {
        FUN <- match.fun(FUN)
        ## Y may have a class, so don't use rep.int
        Y <- rep(Y, rep.int(length(X), length(Y)))
        ##  length.out is not an argument of the generic rep()
        ##  X <- rep(X, length.out = length(Y))
        if(length(X))
            X <- rep(X, times = ceiling(length(Y)/length(X)))
        robj <- FUN(X, Y, ...)
        dim(robj) <- c(dX, dY) # careful not to lose class here
    }
    ## no dimnames if both don't have ..
    if(!(no.nx && no.ny)) {
	if(no.nx) nx <- vector("list", length(dX)) else
	if(no.ny) ny <- vector("list", length(dY))
	dimnames(robj) <- c(nx, ny)
    }
    robj
}

## Binary operator, hence don't simply do "%o%" <- outer.
`%o%` <- function(X, Y) outer(X, Y)
