#  File src/library/graphics/R/identify.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

identify <- function(x, ...) UseMethod("identify")

identify.default <-
    function(x, y = NULL, labels = seq_along(x), pos = FALSE,
             n = length(x), plot = TRUE, atpen = FALSE,
             offset = 0.5, tolerance = 0.25, order = FALSE, ...)
{
    if(length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    xy <- xy.coords(x, y, setLab = FALSE)
    x <- xy$x
    y <- xy$y
    if (length(x) == 0) {
        if (!pos && !order)
            return(numeric())
        else {
            result <- list(ind = numeric())
            if (pos)
                result$pos <- numeric()
            if (order)
                result$order <- numeric()
            return(result)
        }
    }
    z <- .External2(C_identify, x, y, as.character(labels), n, plot,
                    offset, tolerance, atpen)
    i <- seq.int(z[[1L]])[z[[1L]]]
    if (!pos && !order) 
        i
    else {
        result <- list(ind = i)
        if (pos)
            result$pos <- z[[2L]][z[[1L]]]
        if (order)
            result$order <- z[[3L]][z[[1L]]]
        result
    }
}
