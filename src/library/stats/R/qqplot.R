#  File src/library/stats/R/qqplot.R
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

qqplot <- function(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
		   ylab = deparse(substitute(y)), ...)
{
    sx <- sort(x)
    sy <- sort(y)
    lenx <- length(sx)
    leny <- length(sy)
    if( leny < lenx )
	sx <- approx(1L:lenx, sx, n = leny)$y
    if( leny > lenx )
	sy <- approx(1L:leny, sy, n = lenx)$y
    if(plot.it)
	plot(sx, sy, xlab = xlab, ylab = ylab, ...)
    invisible(list(x = sx, y = sy))
}
