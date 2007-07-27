#  File src/library/base/R/diff.R
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

diff <- function(x, ...) UseMethod("diff")

diff.default <- function(x, lag = 1, differences = 1, ...)
{
    ismat <- is.matrix(x)
    xlen <- if(ismat) dim(x)[1] else length(x)
    if (length(lag) > 1 || length(differences) > 1 ||
        lag < 1 || differences < 1)
	stop("'lag' and 'differences' must be integers >= 1")
    if (lag * differences >= xlen)
	return(x[0]) # empty of proper mode
    r <- unclass(x)  # don't want class-specific subset methods
    i1 <- -1:-lag
    if (ismat)
	for (i in 1:differences)
	    r <- r[i1, , drop = FALSE] -
                r[-nrow(r):-(nrow(r)-lag+1), , drop = FALSE]
    else
        for (i in 1:differences)
            r <- r[i1] - r[-length(r):-(length(r)-lag+1)]
    class(r) <- oldClass(x)
    r
}
