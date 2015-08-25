#  File src/library/base/R/which.R
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

which <- function(x, arr.ind = FALSE, useNames = TRUE)
{
    wh <- .Internal(which(x))
    if (arr.ind && !is.null(d <- dim(x)))
	arrayInd(wh, d, dimnames(x), useNames=useNames) else wh
}

arrayInd <- function(ind, .dim, .dimnames = NULL, useNames = FALSE) {
    ##-- return a matrix  length(ind) x rank == length(ind) x length(.dim)
    m <- length(ind)
    rank <- length(.dim)
    wh1 <- ind - 1L
    ind <- 1L + wh1 %% .dim[1L]
    dnms <- if(useNames) {
	list(.dimnames[[1L]][ind],
	     if(any(nzchar(nd <- names(.dimnames)))) nd else
	     if(rank == 2L) c("row", "col") # for matrices
	     else paste0("dim", seq_len(rank)))
    }
    ind <- matrix(ind, nrow = m, ncol = rank, dimnames = dnms)
    if(rank >= 2L) {
	denom <- 1L
	for (i in 2L:rank) {
	    denom <- denom * .dim[i-1L]
	    nextd1 <- wh1 %/% denom	# (next dim of elements) - 1
	    ind[,i] <- 1L + nextd1 %% .dim[i]
	}
    }
    storage.mode(ind) <- "integer"
    ind
}

which.min <- function(x) .Internal(which.min(x))
which.max <- function(x) .Internal(which.max(x))
