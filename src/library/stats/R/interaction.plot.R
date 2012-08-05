#  File src/library/stats/R/interaction.plot.R
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

interaction.plot <-
    function(x.factor, trace.factor, response, fun=mean,
	     type = c("l", "p", "b", "o", "c"), legend = TRUE,
             trace.label=deparse(substitute(trace.factor)), fixed=FALSE,
             xlab = deparse(substitute(x.factor)), ylab = ylabel,
             ylim = range(cells, na.rm=TRUE),
             lty = nc:1, col = 1, pch = c(1L:9, 0, letters),
             xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
             xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ",
                    deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells); nc <- ncol(cells)
    xvals <- 1L:nr
    ## See if the x.factor labels are a sensible scale
    if(is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn=-1)
        xnm <- as.numeric(levels(x.factor))
        options(warn=wn)
        if(!any(is.na(xnm))) xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type="width"))
    if(is.null(xlabs)) xlabs <- as.character(xvals)
    if(is.null(ylabs)) ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if(legend) 0.2 + 0.02*nch else 0.2/nr) * diff(xlim)
    dev.hold(); on.exit(dev.flush())
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
            xlab = xlab, ylab = ylab, axes = axes, xaxt = "n",
            col = col, lty = lty, pch = pch)
    if(axes && xaxt != "n") {
	## swallow ... arguments intended for matplot():
	axisInt <- function(x, main, sub, lwd, bg, log, asp, ...)
	    axis(1, x, ...)
	mgp. <- par("mgp") ; if(!xtick) mgp.[2L] <- 0
	axisInt(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp.,
		xaxt = xaxt, ...)
    }
    if(legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2L] - 0.1 * yrng
        if(!is.null(xpd) || { xpd. <- par("xpd")
                              !is.na(xpd.) && !xpd. && (xpd <- TRUE)}) {
            op <- par(xpd = xpd)
            on.exit(par(op), add = TRUE)
        }
        text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), adj = 0)
        if(!fixed) {
            ## sort them on the value at the last level of x.factor
            ord <- sort.list(cells[nr,  ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1) %% length(lty)]
            col <- col[1 + (ord - 1) %% length(col)]
            pch <- pch[ord]
        }

        legend(xleg, yleg, legend = ylabs, col = col,
               pch = if(type %in% c("p","b")) pch,# NULL works
               lty = if(type %in% c("l","b")) lty,# NULL works
               bty = leg.bty, bg = leg.bg)
    }
    invisible()
}
