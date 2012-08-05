#  File src/library/graphics/R/rug.R
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

rug <- function(x, ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"),
		quiet = getOption("warn") < 0, ...)
{
    x <- as.vector(x)
    ok <- is.finite(x)
    x <- x[ok]
    if(!quiet) {
	u <- par("usr")
	u <- if (side %% 2 == 1) {
	    if(par("xlog")) 10^u[1L:2] else u[1L:2]
	} else {
	    if(par("ylog")) 10^u[3:4] else u[3:4]
	}
	if(any(x < u[1L] | x > u[2L]))
	    warning("some values will be clipped")
    }
    Axis(side = side, at = x, labels = FALSE,
         lwd = 0, lwd.ticks = lwd, col.ticks = col,
         tck = ticksize, ...)
}
