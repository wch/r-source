#  File src/library/graphics/R/spineplot.R
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

## Spine plots/Spinograms contributed by Achim Zeileis

spineplot <- function(x, ...) {
    UseMethod("spineplot")
}

spineplot.formula <-
function(formula, data = NULL,
         breaks = NULL, tol.ylab = 0.05, off = NULL, ylevels = NULL,
         col = NULL, main = "", xlab = NULL, ylab = NULL,
         xaxlabels = NULL, yaxlabels = NULL,
         xlim = NULL, ylim = c(0, 1), axes = TRUE, ...,
         subset = NULL, weights = NULL, drop.unused.levels = FALSE)
{
    ## extract x, y from formula
    m <- match.call(expand.dots = FALSE)
    m <- m[c(1L, match(c("formula", "data", "subset", "weights", "drop.unused.levels"), names(m), 0L))]
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(m)
    if(length(setdiff(names(mf), "(weights)")) != 2L)
        stop("'formula' should specify exactly two variables")
    y <- mf[,1L]
    if(!is.factor(y))
        stop("dependent variable should be a factor")
    if(!is.null(ylevels))
      y <- factor(y, levels = if(is.numeric(ylevels)) levels(y)[ylevels] else ylevels)
    x <- mf[,2L]
    w <- if("(weights)" %in% names(mf)) mf[,"(weights)"] else NULL

    ## graphical parameters
    if(is.null(xlab)) xlab <- names(mf)[2L]
    if(is.null(ylab)) ylab <- names(mf)[1L]

    ## call default interface
    spineplot(x, y, breaks = breaks, tol.ylab = tol.ylab, off = off, ylevels = NULL,
              col = col, main = main, xlab = xlab, ylab = ylab,
              xaxlabels = xaxlabels, yaxlabels = yaxlabels,
              xlim = xlim, ylim = ylim, axes = axes, weights = w, ...)
}

spineplot.default <-
function(x, y = NULL,
         breaks = NULL, tol.ylab = 0.05, off = NULL, ylevels = NULL,
         col = NULL, main = "", xlab = NULL, ylab = NULL,
         xaxlabels = NULL, yaxlabels = NULL,
         xlim = NULL, ylim = c(0, 1), axes = TRUE, weights = NULL, ...)
{
    ## either supply a 2-way table (i.e., both y and x are categorical)
    ## or two variables (y has to be categorical - x can be categorical
    ## or numerical)
    if(missing(y)) {
        if(length(dim(x)) != 2L)
            stop("a 2-way table has to be specified")
        if(!is.null(weights))
            stop("weights are not supported for 2-way table specification")
        tab <- x
        x.categorical <- TRUE
        if(is.null(xlab)) xlab <- names(dimnames(tab))[1L]
        if(is.null(ylab)) ylab <- names(dimnames(tab))[2L]
        xnam <- dimnames(tab)[[1L]]
        ynam <- dimnames(tab)[[2L]]
        ny <- NCOL(tab)
        nx <- NROW(tab)
    } else {
        if(!is.factor(y)) stop("dependent variable should be a factor")
	if(!is.null(ylevels))
          y <- factor(y, levels = if(is.numeric(ylevels)) levels(y)[ylevels] else ylevels)
        x.categorical <- is.factor(x)
        if(is.null(xlab)) xlab <- deparse1(substitute(x))
        if(is.null(ylab)) ylab <- deparse1(substitute(y))
        if(x.categorical) {
            if(is.null(weights)) {
                tab <- table(x, y)
            } else {
                tab <- as.table(tapply(weights, list(x, y), FUN = sum, na.rm = TRUE))
                tab[is.na(tab)] <- 0
            }
            xnam <- levels(x)
            nx <- NROW(tab)
        }
        ynam <- levels(y)
        ny <- length(ynam)
    }

    ## graphical parameters
    if(is.null(col)) col <- gray.colors(ny)
    col <- rep_len(col, ny)
    off <- if(!x.categorical) 0 else if(is.null(off)) 0.02 else off/100
    yaxlabels <- if(is.null(yaxlabels)) ynam else rep_len(yaxlabels, ny)

    if(x.categorical) {
        ## compute rectangle positions on x axis
        xat <- c(0, cumsum(proportions(marginSums(tab, 1)) + off))
        xaxlabels <- if(is.null(xaxlabels)) xnam else rep_len(xaxlabels, nx)
    } else {
        ## handle non-numeric x
	if(!(xnumeric <- is.numeric(x))) {
	    xorig <- x
	    x <- as.numeric(x)
	}
        ## compute breaks for x
        if (is.null(breaks)) {
            breaks <- if(is.null(weights)) nclass.Sturges(x) else ceiling(log2(sum(weights)) + 1)
	}
        breaks <- as.numeric(breaks)
        if (length(breaks) == 1L) {
            if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) {
                stop("invalid number of 'breaks'")
            }
            if (breaks > 1e+06) {
                warning(gettextf("'breaks = %g' is too large and set to 1e6", breaks), domain = NA)
                breaks <- 1000000L
            }
            rg <- if (is.null(weights)) range(x, na.rm = TRUE) else range(x[weights > 0], na.rm = TRUE)
            breaks <- pretty(rg, n = breaks, min.n = 1)        
        }
        ## categorize x
        x1 <- cut(x, breaks = breaks, include.lowest = TRUE)
        ## construct table
        if(is.null(weights)) {
            tab <- table(x1, y)
        } else {
            tab <- as.table(tapply(weights, list(x1, y), FUN = sum, na.rm = TRUE))
            tab[is.na(tab)] <- 0
        }
        ## compute rectangle positions on x axis
        xat <- c(0, cumsum(proportions(marginSums(tab, 1)))) # c(0, cumsum(proportions(table(x1))))
        nx <- NROW(tab)
        xaxlabels <- if(is.null(xaxlabels)) {
	  if(xnumeric) breaks else c(xorig[1L], xorig[c(diff(as.numeric(x1)) > 0, TRUE)])
	} else {
	    rep_len(xaxlabels, nx + 1L)
	}
    }

    ## compute rectangle positions on y axis
    ## (reversing order compared to version R < 4.0.0)
    yaxlabels <- rev(yaxlabels)
    yat <- rbind(0, apply(proportions(tab[, ncol(tab):1L, drop = FALSE], 1), 1L, cumsum))
    yat[is.na(yat)] <- 1

    if(is.null(xlim)) xlim <- c(0, 1 + off * (nx-1L))
    else if(any(xlim < 0) || any(xlim > 1)) {
        warning("x axis is on a cumulative probability scale, 'xlim' must be in [0,1]")
        if(min(xlim) > 1 || max(xlim) < 0) xlim <- c(0, 1)
        else xlim <- c(max(min(xlim), 0), min(max(xlim), 1))
    }
    if(any(ylim < 0) || any(ylim > 1)) {
        warning("y axis is on a cumulative probability scale, 'ylim' must be in [0,1]")
        if(min(ylim) > 1 || max(ylim) < 0) ylim <- c(0, 1)
        else ylim <- c(max(min(ylim), 0), min(max(ylim), 1))
    }

    ## setup plot
    dev.hold(); on.exit(dev.flush())
    plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE,
         xaxs = "i", yaxs = "i", main = main, xlab = xlab, ylab = ylab)

    ## compute coordinates
    ybottom <- as.vector(yat[-(ny + 1L),])
    ytop <- as.vector(yat[-1L,])
    xleft <- rep(xat[1L:nx], rep(ny, nx))
    xright <- rep(xat[2L:(nx+1L)] - off, rep(ny, nx))
    col <- rep(col, nx)

    ## plot rectangles
    rect(xleft, ybottom, xright, ytop, col = col, ...)

    ## axes
    if(axes) {
        ## side --
        ## 1: either numeric or level names
        if(x.categorical)
            axis(1, at = (xat[1L:nx] + xat[2L:(nx+1L)] - off)/2,
                 labels = xaxlabels, tick = FALSE)
        else
            axis(1, at = xat, labels = xaxlabels)

        ## 2: axis with level names of y
        yat <- yat[,1L]
        equidist <- any(diff(yat) < tol.ylab)
        yat <- if(equidist) seq.int(1/(2*ny), 1-1/(2*ny), by = 1/ny)
        else (yat[-1L] + yat[-length(yat)])/2
        axis(2, at = yat, labels = yaxlabels, tick = FALSE)

        ## 3: none
        ## 4: simple numeric
        axis(4)
    }
    if(!x.categorical) box()

    ## return table visualized
    names(dimnames(tab)) <- c(xlab, ylab)
    invisible(tab)
}
