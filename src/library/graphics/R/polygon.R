#  File src/library/graphics/R/polygon.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright 1995-2016 The R Core Team
#  In part (C) 2001 Kevin Buhr
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

### polyhatch -  a pure R implementation of polygon hatching
### Copyright (C) 2001 Kevin Buhr
### Provided to the R project for release under GPL.
### Original nice clean structure destroyed by Ross Ihaka

polygon <-
  function(x, y = NULL, density = NULL, angle = 45,
           border = NULL, col = NA, lty = par("lty"), ..., fillOddEven=FALSE)
{
    ## FIXME: remove this eventually
    ..debug.hatch <- FALSE
    ##-- FIXME: what if `log' is active, for x or y?
    xy <- xy.coords(x, y, setLab = FALSE)

    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(angle) && !is.null(density)) {

        ## hatch helper functions

        polygon.onehatch <-
            function(x, y, x0, y0, xd, yd, ..debug.hatch = FALSE, ...)
        {
            ## draw the intersection of one line with polygon
            ##
            ##  x,y - points of polygon (MUST have first and last points equal)
            ##  x0,y0 - origin of line
            ##  xd,yd - vector giving direction of line
            ##  ... - other parameters to pass to "segments"

            if (..debug.hatch) {
                points(x0, y0)
                arrows(x0, y0, x0 + xd, y0 + yd)
            }

            ## halfplane[i] is 0 or 1 as (x[i], y[i]) lies in left or right
            ##   half-plane of the line

            halfplane <- as.integer(xd * (y - y0) - yd * (x - x0) <= 0)

            ## cross[i] is -1,0, or 1 as segment (x[i], y[i]) -- (x[i+1], y[i+1])
            ##   crosses right-to-left, doesn't cross, or crosses left-to-right

            cross <- halfplane[-1L] - halfplane[-length(halfplane)]
            does.cross <- cross != 0
            if (!any(does.cross)) return() # nothing to draw?

            ## calculate where crossings occur

            x1 <- x[-length(x)][does.cross]; y1 <- y[-length(y)][does.cross]
            x2 <- x[-1L][does.cross]; y2 <- y[-1L][does.cross]

            ## t[i] is "timepoint" on line at which segment (x1, y1)--(x2, y2)
            ##   crosses such that (x0,y0) + t*(xd,yd) is point of intersection

            t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - x1))/
                  (xd * (y2 - y1) - yd * (x2 - x1)))

            ## sort timepoints along line

            o <- order(t)
            tsort <- t[o]

            ## we draw the part of line from t[i] to t[i+1] whenever it lies
            ##   "inside" the polygon --- the definition of this depends on
            ##   fillOddEven:  if FALSE, we crossed
            ##   unequal numbers of left-to-right and right-to-left polygon
            ##   segments to get there.  if TRUE, an odd number of crossings.
            ##

	    crossings <- cumsum(cross[does.cross][o])
	    if (fillOddEven) crossings <- crossings %% 2
            drawline <- crossings != 0

            ## draw those segments

            lx <- x0 + xd * tsort
            ly <- y0 + yd * tsort
            lx1 <- lx[-length(lx)][drawline]; ly1 <- ly[-length(ly)][drawline]
            lx2 <- lx[-1L][drawline]; ly2 <- ly[-1L][drawline]
            segments(lx1, ly1, lx2, ly2, ...)
        }

        polygon.fullhatch <-
            function(x, y, density, angle, ..debug.hatch = FALSE, ...)
        {
            ## draw the hatching for a given polygon
            ##
            ##  x,y - points of polygon (need not have first and last points
            ##        equal, but no NAs are allowed)
            ##  density,angle - of hatching
            ##  ... - other parameters to pass to "segments"

            x <- c(x, x[1L])
            y <- c(y, y[1L])
            angle <- angle %% 180

            if (par("xlog") || par("ylog")) {
                warning("cannot hatch with logarithmic scale active")
                return()
            }
            usr <- par("usr"); pin <- par("pin")

            ## usr coords per inch

            upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L]) / pin

            ## handle "flipped" usr coords

            if (upi[1L] < 0) angle <- 180 - angle
            if (upi[2L] < 0) angle <- 180 - angle
            upi <- abs(upi)

            ## usr-coords direction vector for hatching

            xd <- cos(angle / 180 * pi) * upi[1L]
            yd <- sin(angle / 180 * pi) * upi[2L]

            ## to generate candidate hatching lines for polygon.onehatch,
            ##   we generate those lines necessary to cover the rectangle
            ##   (min(x),min(y)) to (max(x),max(y)) depending on the
            ##   hatching angle

            ## (Note:  We choose hatch line origins such that the hatching,
            ##   if extended outside polygon, would pass through usr-coordinate
            ##   origin.  This ensures that all hatching with same density,
            ##   angle in figure will be aligned.)

            if (angle < 45 || angle > 135) {

                ## first.x and last.x are x-coords of first and last points
                ##  of rectangle to hit, as y-coord moves from bottom up

                if (angle < 45) {
                    first.x <- max(x)
                    last.x <- min(x)
                }
                else {
                    first.x <- min(x)
                    last.x <- max(x)
                }

                ## y.shift is vertical shift between parallel hatching lines

                y.shift <- upi[2L] / density / abs(cos(angle / 180 * pi))

                ## choose line origin (of first line) to align hatching
                ##   with usr origin

                x0 <- 0
                y0 <- floor((min(y) - first.x * yd / xd) / y.shift) * y.shift

                ## line origins above y.end won't hit figure

                y.end <- max(y) - last.x * yd / xd

                ## hatch against all candidate lines

                while (y0 < y.end) {
                    polygon.onehatch(x, y, x0, y0, xd, yd,
                                     ..debug.hatch=..debug.hatch,...)
                    y0 <- y0 + y.shift
                }
            }
            else {
                ## first.y, last.y are y-coords of first and last points
                ##   of rectangle to hit, as x-coord moves from left to right

                if (angle < 90) {
                    first.y <- max(y)
                    last.y <- min(y)
                }
                else {
                    first.y <- min(y)
                    last.y <- max(y)
                }

                ## x.shift is horizontal shift between parallel hatching lines

                x.shift <- upi[1L] / density / abs(sin(angle / 180 * pi))

                ## choose line origin to align with usr origin

                x0 <- floor((min(x) - first.y * xd / yd) / x.shift) * x.shift
                y0 <- 0

                ## line origins to right of x.end won't hit figure

                x.end <- max(x) - last.y * xd / yd

                ## hatch!

                while (x0 < x.end) {
                    polygon.onehatch(x, y, x0, y0, xd, yd,
                                     ..debug.hatch=..debug.hatch,...)
                    x0 <- x0 + x.shift
                }
            }
        }

        ## end of hatch helper functions


        if (missing(col) || is.null(col) || is.na(col)) col <- par("fg")
        if (is.null(border)) border <- col
        if (is.logical(border)) {
            if (!is.na(border) && border) border <- col
            else border <- NA
        }

        ## process multiple polygons separated by NAs

        start <- 1
        ends <- c(seq_along(xy$x)[is.na(xy$x) | is.na(xy$y)], length(xy$x) + 1)

        num.polygons <- length(ends)
        col <- rep_len(col, num.polygons)
        if(length(border))
            border <- rep_len(border, num.polygons)
        if(length(lty))
            lty <- rep_len(lty, num.polygons)
        if(length(density))
            density <- rep_len(density, num.polygons)
        angle <- rep_len(angle, num.polygons)

        i <- 1L
        for (end in ends) {
            if (end > start) {
                if(is.null(density) || is.na(density[i]) || density[i] < 0)
                    .External.graphics(C_polygon, xy$x[start:(end - 1)],
                                       xy$y[start:(end - 1)],
                                       col[i], NA, lty[i], ...)
                else if (density[i] > 0) {

                        ## note: if col[i]==NA, "segments" will fill with par("fg")

                        polygon.fullhatch(xy$x[start:(end - 1)],
                                          xy$y[start:(end - 1)],
                                          col = col[i], lty = lty[i],
                                          density = density[i],
                                          angle = angle[i],
                                          ..debug.hatch = ..debug.hatch, ...)
                    }

                ## compatible with C_polygon:
                ## only cycle through col, lty, etc. on non-empty polygons
                i <- i + 1
            }
            start <- end + 1
        }
        .External.graphics(C_polygon, xy$x, xy$y, NA, border, lty, ...)
    }
    else {
        if (is.logical(border)) {
            if (!is.na(border) && border) border <- par("fg")
            else border <- NA
        }
        .External.graphics(C_polygon, xy$x, xy$y, col, border, lty, ...)
    }
    invisible()
}

xspline <-
  function(x, y = NULL, shape = 0, open = TRUE, repEnds = TRUE,
           draw = TRUE, border = par("fg"), col = NA, ...)
{
    xy <- xy.coords(x, y, setLab = FALSE)
    s <- rep.int(shape, length(xy$x))
    if(open) s[1L] <- s[length(x)] <- 0
    invisible(.External.graphics(C_xspline, xy$x, xy$y, s, open, repEnds,
                                 draw, col, border, ...))
}

polypath <-
  function(x, y = NULL,
           border = NULL, col = NA, lty = par("lty"),
           rule = "winding", ...)
{
    xy <- xy.coords(x, y, setLab = FALSE)
    if (is.logical(border)) {
        if (!is.na(border) && border) border <- par("fg")
        else border <- NA
    }
    rule <- match(rule, c("winding", "evenodd"))
    if (is.na(rule))
        stop("Invalid fill rule for graphics path")
    # Determine path components
    breaks <- which(is.na(xy$x) | is.na(xy$y))
    if (length(breaks) == 0) { # Only one path
        .External.graphics(C_path, xy$x, xy$y,
                           as.integer(length(xy$x)), as.integer(rule),
                           col, border, lty, ...)
    } else {
        nb <- length(breaks)
        lengths <- c(breaks[1] - 1,
                     diff(breaks) - 1,
                     length(xy$x) - breaks[nb])
        .External.graphics(C_path, xy$x[-breaks], xy$y[-breaks],
                           as.integer(lengths), as.integer(rule),
                           col, border, lty, ...)
    }
    invisible()
}

