#  File src/library/base/R/diff.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2026 The R Core Team
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

diff <- function(x, ...) UseMethod("diff")

diff.default <- function(x, lag = 1L, differences = 1L, ...)
{
    ismat <- is.matrix(x)
    if (length(lag) != 1L || length(differences) != 1L ||
        lag < 1L || differences < 1L)
	stop("'lag' and 'differences' must be integers >= 1")
    r <- unclass(x)  # don't want class-specific subset methods
    i1 <- -seq_len(lag)
    if (ismat)
	for (i in seq_len(differences))
	    r <- r[i1, , drop = FALSE] -
		r[seq_len(max(nrow(r) - lag, 0L)), , drop = FALSE] ## `dim<-`(r[...], <dim>) not nice
		## == r[-nrow(r): -max(nrow(r) - lag + 1L, 1L), , drop = FALSE]
    else
	for (i in seq_len(differences))
	    r <- r[i1] - `length<-`(r, max(length(r) - lag, 0L))
    class(r) <- oldClass(x)
    r
}
