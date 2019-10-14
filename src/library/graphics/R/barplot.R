#  File src/library/graphics/R/barplot.R
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

barplot <- function(height, ...) UseMethod("barplot")

barplot.default <-
function(height, width = 1, space = NULL, names.arg = NULL,
	 legend.text = NULL, beside = FALSE, horiz = FALSE,
	 density = NULL, angle = 45,
	 col = NULL, border = par("fg"),
	 main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
	 xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
	 axes = TRUE, axisnames = TRUE,
	 cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
         inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0, add = FALSE,
	 ann = !add && par("ann"),
         args.legend = NULL, ...)
 {
    if (!missing(inside)) .NotYetUsed("inside", error = FALSE)# -> help(.)

    if (is.null(space))
	space <- if (is.matrix(height) && beside) c(0, 1) else 0.2
    space <- space * mean(width)

    if (plot && axisnames && is.null(names.arg))
	names.arg <-
	    if(is.matrix(height)) colnames(height) else names(height)

    vectorInput <- (is.vector(height)
	|| (is.array(height) && (length(dim(height)) == 1)))
	## Treat vectors and 1-d arrays the same.
    if (vectorInput) {
	height <- cbind(height)
	beside <- TRUE
	## The above may look strange, but in particular makes color
	## specs work as most likely expected by the users.
	if(is.null(col)) col <- "grey"
    } else if (is.matrix(height)) {
	## In the matrix case, we use "colors" by default.
	if(is.null(col))
	    col <- gray.colors(nrow(height))
    }
    else
	stop("'height' must be a vector or a matrix")

    if(is.logical(legend.text))
      legend.text <-
	if(legend.text && is.matrix(height)) rownames(height)

    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
	logx <- length(grep("x", log)) > 0L
	logy <- length(grep("y", log)) > 0L
    }
    ## Cannot use rect(*, density=.) when log scales used
    if ((logx || logy) && !is.null(density))
      stop("Cannot use shading lines in bars when log scale is used")

    NR <- nrow(height)
    NC <- ncol(height)

    if (beside) {
	if (length(space) == 2 && !vectorInput)
	    space <- rep.int(c(space[2L], rep.int(space[1L], NR - 1)), NC)
	width <- rep_len(width, NR)
    } else {
	width <- rep_len(width, NC)
    }

    offset <- rep_len(as.vector(offset), length(width))

    delta <- width / 2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta

    log.dat <- (logx && horiz) || (logy && !horiz)# log scale in data direction
    ## check height + offset if using log scale to prevent log(<=0) error
    if (log.dat) {
	if (min(height + offset, na.rm = TRUE) <= 0)
	    stop("log scale error: at least one 'height + offset' value <= 0")
	if (logx && !is.null(xlim) && min(xlim) <= 0)
	    stop("log scale error: 'xlim' <= 0")
	if (logy && !is.null(ylim) && min(ylim) <= 0)
	    stop("log scale error: 'ylim' <= 0")

	## if axis limit is set to < above, adjust bar base value
	## to draw a full bar
	rectbase <-
	    if	    (logy && !horiz && !is.null(ylim))	ylim[1L]
	    else if (logx && horiz  && !is.null(xlim))	xlim[1L]
	    else 0.9 * min(height, na.rm = TRUE)
    } else rectbase <- 0

    ## if stacked bar, set up base/cumsum levels, adjusting for log scale
    if (!beside)
	height <- rbind(rectbase, apply(height, 2L, cumsum))

    rAdj <- offset + (if (log.dat) 0.9 * height else -0.01 * height)

    delta <- width / 2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
	if (is.null(xlim)) xlim <- range(rAdj, height + offset, na.rm = TRUE)
	if (is.null(ylim)) ylim <- c(min(w.l), max(w.r))
    } else {
	if (is.null(xlim)) xlim <- c(min(w.l), max(w.r))
	if (is.null(ylim)) ylim <- range(rAdj, height + offset, na.rm = TRUE)
    }
    if (beside)
	w.m <- matrix(w.m, ncol = NC)
    if(plot) { ##-------- Plotting :
        dev.hold()
	opar <-
	    if (horiz)	par(xaxs = "i", xpd = xpd)
	    else	par(yaxs = "i", xpd = xpd)
	on.exit({dev.flush();par(opar)})

	if (!add) {
	    plot.new()
	    plot.window(xlim, ylim, log = log, ...)
	}

	xyrect <- function(x1,y1, x2,y2, horizontal = TRUE, ...) {
	    if(horizontal)
		rect(x1,y1, x2,y2, ...)
	    else
		rect(y1,x1, y2,x2, ...)
	}
	if (beside)
	    xyrect(rectbase + offset, w.l, c(height) + offset, w.r,
		   horizontal = horiz,
		   angle = angle, density = density,
                   col = col, border = border)
	else {
	    ## noInside <- NC > 1 && !inside # outside border, but not inside
	    ## bordr <- if(noInside) 0 else border
	    for (i in 1L:NC) {
		xyrect(height[1L:NR, i] + offset[i], w.l[i],
		       height[ -1,  i] + offset[i], w.r[i],
		       horizontal = horiz, angle = angle, density = density,
		       col = col, border = border)# = bordr
		## if(noInside)
		##  xyrect(min(height[, i]), w.l[i], max(height[, i]), w.r[i],
		##	   horizontal = horiz, border= border)
	    }
	}
	if (axisnames && !is.null(names.arg)) { # specified or from {col}names
	    at.l <- if (length(names.arg) != length(w.m)) {
		if (length(names.arg) == NC) # i.e. beside (!)
		    colMeans(w.m)
		else
		    stop("incorrect number of names")
	    } else w.m
	    axis(if(horiz) 2 else 1, at = at.l, labels = names.arg,
		 lty = axis.lty, cex.axis = cex.names, ...)
	}
	if(!is.null(legend.text)) {
	    legend.col <- rep_len(col, length(legend.text))
	    if((horiz & beside) || (!horiz & !beside)){
		legend.text <- rev(legend.text)
		legend.col <- rev(legend.col)
		density <- rev(density)
		angle <- rev(angle)
	    }
	    xy <- par("usr")
            if(is.null(args.legend)) {
                legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1),
                       legend = legend.text, angle = angle, density = density,
                       fill = legend.col, xjust = 1, yjust = 1)
            } else {
                args.legend1 <- list(x = xy[2L] - xinch(0.1),
                                     y = xy[4L] - yinch(0.1),
                                     legend = legend.text,
                                     angle = angle, density = density,
                                     fill = legend.col, xjust = 1, yjust = 1)
                args.legend1[names(args.legend)] <- args.legend
                do.call("legend", args.legend1)
            }
	}
	if(ann) title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	if(axes) axis(if(horiz) 1 else 2, cex.axis = cex.axis, ...)
	invisible(w.m)
    } else w.m
}

barplot.formula <- function(formula, data, subset, na.action,
                            horiz = FALSE, xlab = NULL, ylab = NULL, ...)
{
    if (missing(formula) || length(formula) != 3L)
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- m$horiz <- m$xlab <- m$ylab <- NULL
    m[[1L]] <- quote(stats::model.frame)

    mf <- eval(m, parent.frame())
    if (ncol(mf[-1L]) == 0L || ncol(mf[-1L]) >= 3L)
        stop("formula must specify 1 or 2 categorical variables")
    if (anyDuplicated(mf[-1L]))
        stop("duplicated categorical values - try another formula or subset")
    if (horiz) {
        if(is.null(ylab)) ylab <- names(mf)[ncol(mf)]
    } else
        if(is.null(xlab)) xlab <- names(mf)[ncol(mf)]
    if (is.matrix(mf[[1L]])) { ## LHS is cbind()
        if (ncol(mf[-1L]) != 1L)
            stop("formula with cbind() must specify 1 categorical variable")
        lhs <- t(mf[[1L]])
        colnames(lhs) <- mf[[ncol(mf)]]
        barplot.default(lhs, horiz = horiz, xlab = xlab, ylab = ylab, ...)
    } else {
	if (horiz) {
	    if(is.null(xlab)) xlab <- names(mf)[1L]
	} else
	    if(is.null(ylab)) ylab <- names(mf)[1L]
	barplot.default(stats::xtabs(mf, addNA = TRUE),
			horiz = horiz, xlab = xlab, ylab = ylab, ...)
  }
}
