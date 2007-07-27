#  File src/library/graphics/R/locator.R
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

locator <- function(n = 512, type="n", ...)
{
    if(length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    z <- .Internal(locator(n, type=type))# n <= 0 gives error
    x <- z[[1]]
    y <- z[[2]]
    if((n <- z[[3]]) > 0) list(x=x[1:n], y=y[1:n])
}
