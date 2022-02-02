#  File src/library/graphics/R/text.R
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

text <- function(x, ...) UseMethod("text")

text.default <-
function(x, y = NULL, labels = seq_along(x$x),
         adj = NULL, pos = NULL, offset = 0.5,
         vfont = NULL, cex = 1, col = NULL, font = NULL, ...)
{
    if (!missing(y) && (is.character(y) || is.expression(y))) {
	labels <- y; y <- NULL
    }
    x <- xy.coords(x,y, recycle = TRUE, setLab = FALSE)
    labels <- as.graphicsAnnot(labels)
    if (!is.null(vfont))
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface),
                   fontindex = pmatch(vfont[2L], Hershey$fontindex))
    .External.graphics(C_text, x, labels,
                       adj, pos, offset, vfont, cex, col, font, ...)
    invisible()
}
