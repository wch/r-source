#  File src/library/base/R/tapply.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

tapply <- function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)
{
    FUN <- if (!is.null(FUN)) match.fun(FUN)
    if (!is.list(INDEX)) INDEX <- list(INDEX)
    INDEX <- lapply(INDEX, as.factor)
    nI <- length(INDEX)  # now, 'INDEX' is not classed
    if (!nI) stop("'INDEX' is of length zero")
    if (!all(lengths(INDEX) == length(X)))
        stop("arguments must have same length")
    namelist <- lapply(INDEX, levels)#- all of them, yes !
    extent <- lengths(namelist, use.names = FALSE)
    cumextent <- cumprod(extent)
    if (cumextent[nI] > .Machine$integer.max)
        stop("total number of levels >= 2^31")
    storage.mode(cumextent) <- "integer"
    ngroup <- cumextent[nI]
    group <- as.integer(INDEX[[1L]]) #- to contain the splitting vector
    if (nI > 1L)
        for (i in 2L:nI)
           group <- group + cumextent[i - 1L] * (as.integer(INDEX[[i]]) - 1L)
    if (is.null(FUN)) return(group)
    levels(group) <- as.character(seq_len(ngroup))
    class(group) <- "factor"
    ans <- split(X, group) # use generic, e.g. for 'Date'
    names(ans) <- NULL
    index <- as.logical(lengths(ans))  # equivalently, lengths(ans) > 0L
    ans <- lapply(X = ans[index], FUN = FUN, ...)
    ansmat <- array(
	if (simplify && all(lengths(ans) == 1L)) {
	    ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
	    if(!is.null(ans) && is.na(default) && is.atomic(ans))
		vector(typeof(ans))
	    else default
	} else vector("list", prod(extent)),
	dim = extent, dimnames = namelist)
    if(length(ans)) {
	ansmat[index] <- ans
    }
    ansmat
}
