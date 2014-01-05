#  File src/library/graphics/R/cdplot.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

## CD plots contributed by Achim Zeileis

cdplot <- function(x, ...) {
  UseMethod("cdplot")
}

cdplot.formula <-
function(formula, data = list(),
         plot = TRUE, tol.ylab = 0.05, ylevels = NULL,
         bw = "nrd0", n = 512, from = NULL, to = NULL,
         col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
         yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), ...,
         subset = NULL)
{
    ## extract x, y from formula
    m <- match.call(expand.dots = FALSE)
    m <- m[c(1L, match(c("formula", "data", "subset"), names(m), 0L))]
    require(stats, quietly=TRUE)
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(m)
    if(NCOL(mf) != 2L)
        stop("'formula' should specify exactly two variables")
    y <- mf[,1L]
    if(!is.factor(y))
        stop("dependent variable should be a factor")
    if(!is.null(ylevels))
      y <- factor(y, levels = if(is.numeric(ylevels)) levels(y)[ylevels] else ylevels)
    x <- mf[,2L]

    ## graphical parameters
    if(is.null(xlab)) xlab <- names(mf)[2L]
    if(is.null(ylab)) ylab <- names(mf)[1L]
    if(is.null(yaxlabels)) yaxlabels <- levels(y)

    ## call default interface
    cdplot(x, y, plot = plot, tol.ylab = tol.ylab, bw = bw, n = n,
           from = from, to = to, col = col, border = border, main = main,
           xlab = xlab, ylab = ylab, yaxlabels = yaxlabels, xlim = xlim,
           ylim = ylim, ...)
}

cdplot.default <-
function(x, y,
         plot = TRUE, tol.ylab = 0.05, ylevels = NULL,
         bw = "nrd0", n = 512, from = NULL, to = NULL,
         col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
         yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), ...)
{
    ## graphical parameters
    if(is.null(xlab)) xlab <- deparse(substitute(x))
    if(is.null(ylab)) ylab <- deparse(substitute(y))
    if(is.null(col)) col <- gray.colors(length(levels(y)))
    col <- rep_len(col, length.out = length(levels(y)))
    if(is.null(yaxlabels)) yaxlabels <- levels(y)

    ## coerce x and check y
    xorig <- x
    x <- as.numeric(x)
    if(!is.factor(y)) stop("dependent variable should be a factor")
    if(!is.null(ylevels))
      y <- factor(y, levels = if(is.numeric(ylevels)) levels(y)[ylevels] else ylevels)

    ## unconditional density of x
    dx <- if(is.null(from) & is.null(to))
        stats::density(x, bw = bw, n = n, ...)
    else
        stats::density(x, bw = bw, from = from, to = to, n = n, ...)
    x1 <- dx$x

    ## setup conditional values
    ny <- length(levels(y))
    yprop <- cumsum(prop.table(table(y)))
    y1 <- matrix(rep(0, n * (ny - 1L)), nrow = (ny - 1L))

    ## setup return value
    rval <- list()

    for(i in seq_len(ny-1L)) {
        dxi <- stats::density(x[y %in% levels(y)[seq_len(i)]], bw = dx$bw, n = n,
                              from = min(dx$x), to = max(dx$x), ...)
        y1[i,] <- dxi$y/dx$y * yprop[i]
        rval[[i]] <- stats::approxfun(x1, y1[i,], rule = 2)
    }
    names(rval) <- levels(y)[seq_len(ny-1L)]

    ## use known ranges
    y1 <- rbind(0, y1, 1)
    y1 <- y1[,which(x1 >= min(x) & x1 <= max(x))]
    x1 <- x1[x1 >= min(x) & x1 <= max(x)]

    if(is.null(xlim)) xlim <- range(x1)
    if(any(ylim < 0) || any(ylim > 1)) {
        warning("y axis is on a cumulative probability scale, 'ylim' must be in [0,1]")
        if(min(ylim) > 1 || max(ylim) < 0) ylim <- c(0, 1)
        else ylim <- c(max(min(ylim), 0), min(max(ylim), 1))
    }

    ## plot polygons
    if(plot) {
        dev.hold(); on.exit(dev.flush())
        plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE,
             xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab, main = main)
        for(i in seq_len(NROW(y1) - 1L))
            polygon(c(x1, rev(x1)), c(y1[i+1,], rev(y1[i,])), col = col[i],
                    border = border)
        Axis(xorig, side = 1)

        equidist <- any(diff(y1[,1L]) < tol.ylab)
        if(equidist)
            axis(2, at = seq.int(1/(2*ny), 1-1/(2*ny), by = 1/ny), labels = yaxlabels, tick = FALSE)
        else
            axis(2, at = (y1[-1L,1L] + y1[-NROW(y1), 1L])/2, labels = yaxlabels, tick = FALSE)
        axis(4)
        box()
    }

    ## return conditional density functions
    invisible(rval)
}

