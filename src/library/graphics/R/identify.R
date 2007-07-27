#  File src/library/graphics/R/identify.R
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

identify <- function(x, ...) UseMethod("identify")

identify.default <-
    function(x, y = NULL, labels = seq_along(x), pos = FALSE,
             n = length(x), plot = TRUE, atpen = FALSE,
             offset = 0.5, tolerance = 0.25, ...)
{
    if(length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    if (length(x)==0) {
        if (pos)
            return(list(ind=numeric(0), pos=numeric(0)))
        else
            return(numeric(0))
    }
    z <- .Internal(identify(x, y, as.character(labels), n, plot, offset,
                            tolerance, atpen))
    i <- seq.int(z[[1]])[z[[1]]]
    if(pos) list(ind = i, pos = z[[2]][z[[1]]]) else i
}
