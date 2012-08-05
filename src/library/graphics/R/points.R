#  File src/library/graphics/R/points.R
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

points <- function(x, ...) UseMethod("points")

points.default <- function(x, y=NULL, type="p", ...)
    plot.xy(xy.coords(x,y), type=type, ...)

points.table <- function (x, y = NULL, type = "h", lwd = 2, ...)
{
     if (is.null(y) && length(dim(x)) == 1L) {
         nx <- dimnames(x)[[1L]]
         is.num <- suppressWarnings(!any(is.na(xx <- as.numeric(nx))))
         x0 <- if (is.num) xx else seq.int(x)
         points(x0, unclass(x), type = type, lwd = lwd, ...)
     }
     else stop("only for 1-D table")
}

## points.formula  --> ./plot.R
