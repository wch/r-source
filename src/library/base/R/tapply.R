#  File src/library/base/R/tapply.R
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

tapply <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE)
{
    FUN <- if (!is.null(FUN)) match.fun(FUN)
    if (!is.list(INDEX)) INDEX <- list(INDEX)
    nI <- length(INDEX)
    if (!nI) stop("'INDEX' is of length zero")
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- 1L
    group <- rep.int(one, nx) #- to contain the splitting vector
    ngroup <- one
    for (i in seq_along(INDEX)) {
	index <- as.factor(INDEX[[i]])
	if (length(index) != nx)
	    stop("arguments must have same length")
	namelist[[i]] <- levels(index)#- all of them, yes !
	extent[i] <- nlevels(index)
	group <- group + ngroup * (as.integer(index) - one)
	ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN)) return(group)
    ans <- lapply(X = split(X, group), FUN = FUN, ...)
    index <- as.integer(names(ans))
    if (simplify && all(lengths(ans) == 1L)) {
	ansmat <- array(dim = extent, dimnames = namelist)
	ans <- unlist(ans, recursive = FALSE)
    } else {
	ansmat <- array(vector("list", prod(extent)),
			dim = extent, dimnames = namelist)
    }
    if(length(index)) {
        names(ans) <- NULL
        ansmat[index] <- ans
    }
    ansmat
}





