#  File src/library/graphics/R/grid.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

.grid.at <- function(side, n, log, equilogs, axp, usr2, nintLog = NULL) {
    ## return 'at'
    if(is.null(n)) { ## align to tickmarks
        stopifnot(is.numeric(ax <- axp), length(ax) == 3L)
        if(log && equilogs && ax[3L] > 0) ax[3L] <- 1
        axTicks(side, axp = ax, usr=usr2, log=log, nintLog = nintLog)
    } else if(!is.na(n) && (n <- as.integer(n)) >= 1L) { # equidistant, also from box borders
        at <- seq.int(usr2[1L], usr2[2L], length.out = n+1L)
        (if(log) 10^at else at)[-c(1L,n+1L)]
    } # else NULL
}

grid <- function (nx = NULL, ny = nx, col="lightgray", lty="dotted",
                  lwd = par("lwd"), equilogs = TRUE, nintLog = NULL)
{
    atx <- if(is.null(nx) || (!is.na(nx) && nx >= 1))
	       .grid.at(1L, nx, log = par("xlog"), equilogs, axp = par("xaxp"), usr2 = par("usr")[1:2],
                        nintLog = nintLog[1])
    ## else NULL
    aty <- if(is.null(ny) || (!is.na(ny) && ny >= 1))
	       .grid.at(2L, ny, log = par("ylog"), equilogs, axp = par("yaxp"), usr2 = par("usr")[3:4],
                        nintLog = nintLog[min(2L, length(nintLog))])
    abline(v = atx, h = aty, col = col, lty = lty, lwd = lwd)
    invisible(list(atx = atx, aty = aty))
}
