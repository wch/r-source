#  File src/library/base/R/range.R
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

range.default <- function(..., na.rm = FALSE, finite = FALSE)
{
    x <- c(..., recursive = TRUE)
    if(is.numeric(x)) {
        if(finite) x <- x[is.finite(x)]
        else if(na.rm) x <- x[!is.na(x)]
    }
    c(min(x), max(x)) # even if x is empty from 1.5.0
}
