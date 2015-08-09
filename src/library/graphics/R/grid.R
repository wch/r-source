#  File src/library/graphics/R/grid.R
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

grid <- function (nx = NULL, ny = nx, col="lightgray", lty="dotted",
                  lwd = par("lwd"), equilogs = TRUE)
{
    if(is.null(nx) || (!is.na(nx) && nx >= 1)) {
        log <- par("xlog")
        if(is.null(nx)) { ## align to tickmarks
            ax <- par("xaxp")
            if(log && equilogs && ax[3L] > 0) ax[3L] <- 1
            at <- axTicks(1, axp = ax, log=log)
        } else { # equidistant, also from box borders
            U <- par("usr")
            at <- seq.int(U[1L],U[2L], length.out = nx+1)
            at <- (if(log) 10^at else at)[-c(1,nx+1)]
        }
        abline(v = at, col = col, lty = lty, lwd = lwd)
    }
    if(is.null(ny) || (!is.na(ny) && ny >= 1)) {
        log <- par("ylog")
        if(is.null(ny)) { ## align to tickmarks
            ax <- par("yaxp")
            if(log && equilogs && ax[3L] > 0) ax[3L] <- 1
            at <- axTicks(2, axp = ax, log=log)
        } else { # equidistant, also from box borders
            U <- par("usr")
            at <- seq.int(U[3L],U[4L], length.out = ny+1)
            at <- (if(log) 10^at else at)[-c(1,ny+1)]
        }
	abline(h = at, col = col, lty = lty, lwd = lwd)
    }
}
