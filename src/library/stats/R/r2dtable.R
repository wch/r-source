#  File src/library/stats/R/r2dtable.R
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

r2dtable <- function(n, r, c)
{
    if(length(n) == 0L || (n < 0) || is.na(n))
	stop("invalid argument 'n'")
    if((length(r) <= 1L) || any(r < 0) || anyNA(r))
	stop("invalid argument 'r'")
    if((length(c) <= 1L) || any(c < 0) || anyNA(c))
	stop("invalid argument 'c'")
    if(sum(r) != sum(c))
	stop("arguments 'r' and 'c' must have the same sums")
    .Call(C_r2dtable, as.integer(n), as.integer(r), as.integer(c))
}
