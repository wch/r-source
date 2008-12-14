#  File src/library/base/R/array.R
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

array <-
function(data = NA, dim = length(data), dimnames = NULL)
{
    data <- as.vector(data)
    dim <- as.integer(dim)
    vl <- prod(dim)
    if(length(data) != vl) {
        if(vl > .Machine$integer.max)
            stop("'dim' specifies too large an array")
        data <- rep(data, length.out=vl)
    }
    if(length(dim))
	dim(data) <- dim
    if(is.list(dimnames) && length(dimnames))
	dimnames(data) <- dimnames
    data
}

slice.index <-
function(x, MARGIN)
{
    d <- dim(x)
    if(is.null(d))
        d <- length(x)
    n <- length(d)

    if((length(MARGIN) > 1L) || (MARGIN < 1L) || (MARGIN > n))
        stop("incorrect value for 'MARGIN'")

    if(any(d == 0L)) return(array(integer(0L), d))

    y <- rep.int(rep.int(1L:d[MARGIN],
			 prod(d[seq_len(MARGIN - 1L)]) * rep.int(1L, d[MARGIN])),
		 prod(d[seq.int(from = MARGIN + 1L, length.out = n - MARGIN)]))
    dim(y) <- d
    y
}
