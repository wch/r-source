#  File src/library/base/R/which.R
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

which <- function(x, arr.ind = FALSE)
{
    if(!is.logical(x))
	stop("argument to 'which' is not logical")
    wh <- seq_along(x)[x & !is.na(x)]
    dl <- dim(x)
    if (is.null(dl) || !arr.ind) {
	names(wh) <- names(x)[wh]
    }
    else { ##-- return a matrix  length(wh) x rank
        m <- length(wh)
        rank <- length(dl)
        wh1 <- wh - 1
        wh <- 1 + wh1 %% dl[1]
        wh <- matrix(wh, nrow = m, ncol = rank,
                     dimnames =
                     list(dimnames(x)[[1]][wh],
                          if(rank == 2) c("row", "col")# for matrices
                          else paste("dim", 1:rank, sep="")))
        if(rank >= 2) {
            denom <- 1
            for (i in 2:rank) {
                denom <- denom * dl[i-1]
                nextd1 <- wh1 %/% denom# (next dim of elements) - 1
                wh[,i] <- 1 + nextd1 %% dl[i]
            }
        }
        storage.mode(wh) <- "integer"
    }
    wh
}

which.min <- function(x) .Internal(which.min(x))
which.max <- function(x) .Internal(which.max(x))

