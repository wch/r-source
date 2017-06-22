#  File src/library/base/R/diag.R
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

diag <- function(x = 1, nrow, ncol, names = TRUE)
{
    if (is.matrix(x)) {
	if (nargs() > 1L &&
	    (nargs() > 2L || any(names(match.call()) %in% c("nrow", "ncol"))))
            stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix")

        if((m <- min(dim(x))) == 0L) return(vector(typeof(x), 0L))
        ## NB: need double index to avoid overflows.
        y <- x[1 + 0L:(m - 1L) * (dim(x)[1L] + 1)]
	if(names) {
	    nms <- dimnames(x)
	    if (is.list(nms) && !any(vapply(nms, is.null, NA)) &&
		identical((nm <- nms[[1L]][seq_len(m)]), nms[[2L]][seq_len(m)]))
		names(y) <- nm
	}
        return(y)
    }
    if (is.array(x) && length(dim(x)) != 1L)
        stop("'x' is an array, but not one-dimensional.")

    if (missing(x)) n <- nrow
    else if (length(x) == 1L && nargs() == 1L) {
	n <- as.integer(x)
	x <- 1
    } else n <- length(x)
    if (!missing(nrow)) n <- nrow
    if (missing(ncol)) ncol <- n
    ## some people worry about speed
    .Internal(diag(x, n, ncol))
}

`diag<-` <- function(x, value)
{
    dx <- dim(x)
    if (length(dx) != 2L)
	## no further check, to also work with 'Matrix'
	stop("only matrix diagonals can be replaced")
    len.i <- min(dx)
    len.v <- length(value)
    if (len.v != 1L && len.v != len.i)
	stop("replacement diagonal has wrong length")
    if (len.i) {
	i <- seq_len(len.i)
	x[cbind(i, i)] <- value
    }
    x
}
