#  File src/library/graphics/R/dotchart.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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

dotchart <-
function(x, labels = NULL, groups = NULL, gdata = NULL, offset = 1/8,
         ann = par("ann"), xaxt = par("xaxt"), frame.plot = TRUE, log = "",
         cex = par("cex"), pt.cex = cex,
	 pch = 21, gpch = 21, bg = par("bg"),
	 color = par("fg"), gcolor = par("fg"), lcolor = "gray",
	 xlim = range(x[is.finite(x)]),
	 main = NULL, xlab = NULL, ylab = NULL, ...)
{
    ## old-style "graphics" design-bug: ("mar"), ("mai"), ("mar", "mai")
    ##			    all fail, just the following, ("mai", "mar") is ok:
    opar <- par("mai", "mar", "mgp", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")

    if(!is.numeric(x))
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
	if (is.null(labels))
	    labels <- rownames(x)
	if (is.null(labels))
	    labels <- as.character(seq_len(nrow(x)))
	labels <- rep_len(labels, n)
	if (is.null(groups))
	    groups <- col(x, as.factor = TRUE)
	glabels <- levels(groups)
    } else {
	if (is.null(labels)) labels <- names(x)
	glabels <- if(!is.null(groups)) levels(groups)
        if (!is.vector(x)) { # e.g. a table
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
    }

    plot.new() # for strwidth()

    linch <-
	if(!is.null(labels)) max(strwidth(labels, "inch"), na.rm = TRUE) else 0
    if (is.null(glabels)) {
	ginch <- 0
	goffset <- 0
    }
    else {
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
	goffset <- offset
    }
    nmai <- opar[["mai"]]
    if(ann)
        nm.2 <- nmai[2L]
    if (!(is.null(labels) && is.null(glabels))) {
        ## The intention seems to be to balance the whitespace
        ## on each side (2 & 4) of the labels+plot.
	yi <- if(is.null(ylab) || !ann) 0 else offset
	nm.2 <- nmai[4L] + max(yi + linch + goffset, ginch) + 1/16
	if (nmai[2L] <	nm.2) { ## add space for ylab + glabels on left margin
	    nmai[2L] <- nm.2
	    par(mai = nmai)
	}
    }

    if (is.null(groups)) {
	o <- seq_len(n)
	y <- o
	ylim <- c(0, n + 1)
    }
    else {
	o <- sort.list(as.numeric(groups), decreasing = TRUE)
	x      <- x     [o]
	groups <- groups[o]
	color  <- rep_len(color,  length(groups))[o]
	lcolor <- rep_len(lcolor, length(groups))[o]
	pch    <- rep_len(pch,    length(groups))[o]
	of.1 <- cumsum(c(0, diff(as.numeric(groups)) != 0))
	y <- seq_len(n) + 2 * of.1
	ylim <- range(0, y + 2)
    }

    plot.window(xlim = xlim, ylim = ylim, log = log)
#    xmin <- par("usr")[1L]
    lheight <- par("csi")
    if (!is.null(labels)) {
	loffset <- (linch + 0.1)/lheight
        mtext(labels[o], side = 2, line = loffset, at = y, adj = 0,
              col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
    if (!is.null(groups)) {
	gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 2) - 1)
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
	goffset <- (max(linch+offset, ginch, na.rm = TRUE) + 1/16)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos,
              adj = 0, col = gcolor, las = 2, cex = cex, ...)
	if (!is.null(gdata)) {
	    abline(h = gpos, lty = "dotted")
	    points(gdata, gpos, pch = gpch, col = gcolor, bg = bg,
                   cex = pt.cex/cex, ...)
	}
    }
    axis(1, xaxt=xaxt) # FIXME? add '...' or use localAxis() as plot.default()
    if(frame.plot)
	box()
    if(ann) {
	title(main=main, xlab=xlab, ...) # with default "mgp"
	## y-axis label must be left of the (regular + group) labels:
	mgp <- par("mgp")
	par(mgp = c(max(mgp[1], nm.2 / lheight - 1.5), mgp[-1]))
	title(ylab=ylab, ...)
    }
    invisible()
}
