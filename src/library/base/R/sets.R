#  File src/library/base/R/sets.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

## See the help for why as.vector is used:
## it includes coercing factors.
union <- function(x, y) unique(c(as.vector(x), as.vector(y)))

intersect <- function(x, y)
{
    y <- as.vector(y)
    unique(y[match(as.vector(x), y, 0L)])
}

setdiff <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    unique(if(length(x) || length(y)) x[match(x, y, 0L) == 0L] else x)
}
## Faster versions, see R-devel, Jan.4-6, 2000;  optimize later...
setequal <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    all(c(match(x, y, 0L) > 0L, match(y, x, 0L) > 0L))
}

##  same as %in% ( ./match.R ) but different arg names:
is.element <- function(el, set) match(el, set, 0L) > 0L
