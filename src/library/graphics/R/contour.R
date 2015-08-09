#  File src/library/graphics/R/contour.R
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

contour <- function(x, ...) UseMethod("contour")

contour.default <-
function (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)),
	  z,
	  nlevels = 10, levels = pretty(zlim, nlevels), labels = NULL,
	  xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
	  zlim = range(z, finite = TRUE),
	  labcex = 0.6, drawlabels = TRUE, method = "flattest",
          vfont, axes = TRUE, frame.plot = axes,
	  col = par("fg"), lty = par("lty"), lwd = par("lwd"),
	  add = FALSE, ...)
{
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		z <- x
		x <- seq.int(0, 1, length.out = nrow(z))
	    }
	} else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
	y <- x$y
	x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L)
	stop("no proper 'z' matrix specified")
    if (!add) {
        localPlotWindow <-
            function(xlim, ylim, ..., main, sub, xlab, ylab, outer, line)
                plot.window(xlim, ylim, ...)
        localTitle <- function(..., log) title(...)
	plot.new()
	localPlotWindow(xlim, ylim, ...)
	localTitle(...)
    }
    ##- don't lose  dim(.)
    method <- pmatch(method[1L], c("simple", "edge", "flattest"))
    if (missing(vfont))
       vfont <- if(.Call(C_contourDef)) NULL else c("sans serif", "plain")
    if (!is.null(vfont))
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface),
                   fontindex = pmatch(vfont[2L], Hershey$fontindex))
    if (!is.null(labels))
        labels <- as.character(labels)
    .External.graphics(C_contour, x, y, z, levels, labels, labcex, drawlabels,
                       method, vfont, col, lty, lwd)
    if(!add) {
        ## at least col, lty, lwd are not needed,
        ## but easiest to be consistent with plot.default
        localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
        localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
        if(axes) {
            localAxis(x, side = 1, ...)
            localAxis(y, side = 2, ...)
        }
        if(frame.plot) localBox(...)
    }
    invisible()
}
