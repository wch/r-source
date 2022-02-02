#  File src/library/graphics/R/rect.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

rect <-
  function (xleft, ybottom, xright, ytop, density = NULL, angle = 45,
            col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...)
{
    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border) && !is.na(border)) {
            if (border) border <- col
            else border <- NA
        }
        n <- range(length(xleft), length(xright),
                   length(ybottom), length(ytop))
        if (n[1L] == 0)
            stop("invalid rectangle specification")
        n <- n[2L]
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1L]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1L]
        polygon(x, y, col = col, border = border, lty = lty, lwd = lwd,
                density = density, angle = angle, ...)
    }
    else
        .External.graphics(C_rect, as.double(xleft), as.double(ybottom),
                           as.double(xright), as.double(ytop),
                           col = col, border = border,
                           lty = lty, lwd = lwd, ...)
    invisible()
}
