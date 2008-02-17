#  File src/library/base/R/diag.R
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

diag <- function(x = 1, nrow, ncol)
{
    if (is.matrix(x) && nargs() == 1) {
        if((m <- min(dim(x))) == 0)
            return(numeric(0))

        y <- c(x)[1 + 0:(m - 1) * (dim(x)[1] + 1)]
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) &&
            identical((nm <- nms[[1]][1:m]), nms[[2]][1:m]))
            names(y) <- nm
        return(y)
    }
    if(is.array(x) && length(dim(x)) != 1)
        stop("first argument is array, but not matrix.")

    if(missing(x))
	n <- nrow
    else if(length(x) == 1 && missing(nrow) && missing(ncol)) {
	n <- as.integer(x)
	x <- 1
    }
    else n <- length(x)
    if(!missing(nrow))
	n <- nrow
    if(missing(ncol))
	ncol <- n
    p <- ncol
    y <- array(0, c(n, p))
    if((m <- min(n, p)) > 0) y[1 + 0:(m - 1) * (n + 1)] <- x
    y
}

"diag<-" <- function(x, value)
{
    dx <- dim(x)
    if(length(dx) != 2)
	## no further check, to also work with 'Matrix'
	stop("only matrix diagonals can be replaced")
    len.i <- min(dx)
    i <- seq_len(len.i)
    len.v <- length(value)
    if(len.v != 1 && len.v != len.i)
	stop("replacement diagonal has wrong length")
    if(len.i > 0) x[cbind(i, i)] <- value
    x
}
