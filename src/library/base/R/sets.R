#  File src/library/base/R/sets.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

## speed optimization etc: R-devel, Jan.4-6, 2000; then again 15 yrs later
setequal <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    !( anyNA(match(x, y)) || anyNA(match(y, x)) )
}

##  same as %in% ( ./match.R ) but different arg names:
is.element <- function(el, set) match(el, set, 0L) > 0L
