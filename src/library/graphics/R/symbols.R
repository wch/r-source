#  File src/library/graphics/R/symbols.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

symbols <-
function (x, y = NULL, circles, squares, rectangles, stars,
	  thermometers, boxplots, inches = TRUE, add = FALSE,
	  fg = par("col"), bg = NA, xlab = NULL, ylab = NULL, main = NULL,
	  xlim=NULL, ylim=NULL, ...)
{
    count <- 0
    if (!missing(circles)) {
	count <- count + 1
	data <- circles
	type <- 1
    }
    if (!missing(squares)) {
	count <- count + 1
	data <- squares
	type <- 2
    }
    if (!missing(rectangles)) {
	count <- count + 1
	data <- rectangles
	type <- 3
    }
    if (!missing(stars)) {
	count <- count + 1
	data <- stars
	type <- 4
    }
    if (!missing(thermometers)) {
	count <- count + 1
	data <- thermometers
	type <- 5
    }
    if (!missing(boxplots)) {
	count <- count + 1
	data <- boxplots
	type <- 6
    }
    if (count != 1)
	stop("exactly one symbol type must be specified")
    xy <- xy.coords(x, y,
                    xlab = deparse1(substitute(x)),
                    ylab = deparse1(substitute(y)))
    x <- xy$x; y <- xy$y
    if (!add) {
	if(is.null(xlab)) xlab <- xy$xlab
	if(is.null(ylab)) ylab <- xy$ylab
	## Expand the range by 2 * 0.10 = 20% : wild guess !
	## FIXME: better guess: use size of largest symbol...
	##	      really would need	 (x, y, type, data, inches) ->
	##	      rather an internal symbols.limits()
	if(is.null(xlim))
	    xlim <- extendrange(x, f = 0.10)
	if(is.null(ylim))
	    ylim <- extendrange(y, f = 0.10)
	plot(NA, NA, type = "n", xlim = xlim, ylim = ylim,
	     xlab = xlab, ylab = ylab, main = main, ...)
    }
    invisible(.External.graphics(C_symbols, x, y, type, data, inches, bg, fg, ...))
}
