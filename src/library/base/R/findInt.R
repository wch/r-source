#  File src/library/base/R/findInt.R
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

### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop("'vec' contains NAs")
    if(is.unsorted(vec))
	stop("'vec' must be sorted non-decreasingly")
    ## deal with NA's in x:
    if(has.na <- any(ix <- is.na(x))) x <- x[!ix]
    nx <- as.integer(length(x))
    if (is.na(nx)) stop("invalid length(x)")
    nv <- as.integer(length(vec))
    if (is.na(nv)) stop("invalid length(vec)")
    index <- integer(nx)
    ## NB: this is naughty, and changes index in-place.
    .C("find_interv_vec",
       xt = as.double(vec), n = nv,
       x  = as.double(x),  nx = nx,
       as.logical(rightmost.closed),
       as.logical(all.inside),
       index, DUP = FALSE, NAOK = TRUE, # NAOK: 'Inf' only
       PACKAGE = "base")
    if(has.na) {
	ii <- as.integer(ix)
	ii[ix] <- NA
	ii[!ix] <- index
	ii
    } else index
}
