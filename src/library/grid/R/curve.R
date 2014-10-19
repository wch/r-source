#  File src/library/grid/R/curve.R
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


###############################
# CURVE primitive
###############################

calcOrigin <- function(x1, y1, x2, y2, origin, hand) {
    # Positive origin means origin to the "right"
    # Negative origin means origin to the "left"
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    slope <- dy/dx
    oslope <- -1/slope
    # The origin is a point somewhere along the line between
    # the end points, rotated by 90 (or -90) degrees
    # Two special cases:
    # If slope is non-finite then the end points lie on a vertical line, so
    # the origin lies along a horizontal line (oslope = 0)
    # If oslope is non-finite then the end points lie on a horizontal line,
    # so the origin lies along a vertical line (oslope = Inf)
    tmpox <- ifelse(!is.finite(slope),
                    xm,
                    ifelse(!is.finite(oslope),
                           xm + origin*(x2 - x1)/2,
                           xm + origin*(x2 - x1)/2))
    tmpoy <- ifelse(!is.finite(slope),
                    ym + origin*(y2 - y1)/2,
                    ifelse(!is.finite(oslope),
                           ym,
                           ym + origin*(y2 - y1)/2))
    # ALWAYS rotate by -90 about midpoint between end points
    # Actually no need for "hand" because "origin" also
    # encodes direction
    # sintheta <- switch(hand, left=-1, right=1)
    sintheta <- -1
    ox <- xm - (tmpoy - ym)*sintheta
    oy <- ym + (tmpox - xm)*sintheta

    list(x=ox, y=oy)
}

# Given ncp*ncurve vector of values, ncurve vector of start values,
# ncurve vector of end values, ncurve vector of end logicals,
# combine start or end values with original values based on logicals
interleave <- function(ncp, ncurve, val, sval, eval, e) {
    sval <- rep(sval, length.out=ncurve)
    eval <- rep(eval, length.out=ncurve)
    result <- matrix(NA, ncol=ncurve, nrow=ncp+1)
    m <- matrix(val, ncol=ncurve)
    for (i in 1L:ncurve) {
        if (e[i])
            result[,i] <- c(m[,i], eval[i])
        else
            result[,i] <- c(sval[i], m[,i])
    }
    as.numeric(result)
}

# Calculate a "square" set of end points to calculate control points from
# NOTE: end points may be vector
calcSquareControlPoints <- function(x1, y1, x2, y2,
                                    curvature, angle, ncp,
                                    debug=FALSE) {
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    slope <- dy/dx
    oslope <- -1/slope

    # FIXME:  There MUST be a more compact way of calculating the
    # new end point!
    end <- (slope > 1 |
            (slope < 0 & slope > -1))
    if (curvature < 0)
        end <- !end
    startx <- ifelse(end,
                     x1,
                     ifelse(abs(slope) > 1, x2 - dx, x2 - sign(slope)*dy))
    starty <- ifelse(end,
                     y1,
                     ifelse(abs(slope) > 1, y2 - sign(slope)*dx, y2 - dy))
    endx <- ifelse(end,
                   ifelse(abs(slope) > 1, x1 + dx, x1 + sign(slope)*dy),
                   x2)
    endy <- ifelse(end,
                   ifelse(abs(slope) > 1, y1 + sign(slope)*dx, y1 + dy),
                   y2)

    cps <- calcControlPoints(startx, starty, endx, endy,
                             curvature, angle, ncp,
                             debug)

    # Intereave control points and extra "square" control points
    ncurve <- length(x1)
    cps$x <- interleave(ncp, ncurve, cps$x, startx, endx, end)
    cps$y <- interleave(ncp, ncurve, cps$y, starty, endy, end)

    list(x=cps$x, y=cps$y, end=end)
}

# Find origin of rotation
# Rotate around that origin
calcControlPoints <- function(x1, y1, x2, y2, curvature, angle, ncp,
                              debug=FALSE) {
    # Negative curvature means curve to the left
    # Positive curvature means curve to the right
    # Special case curvature = 0 (straight line) has been handled
    xm <- (x1 + x2)/2
    ym <- (y1 + y2)/2
    dx <- x2 - x1
    dy <- y2 - y1
    slope <- dy/dx

    # Calculate "corner" of region to produce control points in
    # (depends on 'angle', which MUST lie between 0 and 180)
    # Find by rotating start point by angle around mid point
    if (is.null(angle)) {
        # Calculate angle automatically
        angle <- ifelse(slope < 0,
                        2*atan(abs(slope)),
                        2*atan(1/slope))
    } else {
        angle <- angle/180*pi
    }
    sina <- sin(angle)
    cosa <- cos(angle)
    # FIXME:  special case of vertical or horizontal line ?
    cornerx <- xm + (x1 - xm)*cosa - (y1 - ym)*sina
    cornery <- ym + (y1 - ym)*cosa + (x1 - xm)*sina

    # Debugging
    if (debug) {
        grid.points(cornerx, cornery, default.units="inches",
                    pch=16, size=unit(3, "mm"),
                    gp=gpar(col="grey"))
    }

    # Calculate angle to rotate region by to align it with x/y axes
    beta <- -atan((cornery - y1)/(cornerx - x1))
    sinb <- sin(beta)
    cosb <- cos(beta)
    # Rotate end point about start point to align region with x/y axes
    newx2 <- x1 + dx*cosb - dy*sinb
    newy2 <- y1 + dy*cosb + dx*sinb

    # Calculate x-scale factor to make region "square"
    # FIXME:  special case of vertical or horizontal line ?
    scalex <- (newy2 - y1)/(newx2 - x1)
    # Scale end points to make region "square"
    newx1 <- x1*scalex
    newx2 <- newx2*scalex

    # Calculate the origin in the "square" region
    # (for rotating start point to produce control points)
    # (depends on 'curvature')
    # 'origin' calculated from 'curvature'
    ratio <- 2*(sin(atan(curvature))^2)
    origin <- curvature - curvature/ratio
    # 'hand' also calculated from 'curvature'
    if (curvature > 0)
        hand <- "right"
    else
        hand <- "left"
    oxy <- calcOrigin(newx1, y1, newx2, newy2, origin, hand)
    ox <- oxy$x
    oy <- oxy$y

    # Calculate control points
    # Direction of rotation depends on 'hand'
    dir <- switch(hand,
                  left=-1,
                  right=1)
    # Angle of rotation depends on location of origin
    maxtheta <- pi + sign(origin*dir)*2*atan(abs(origin))
    theta <- seq(0, dir*maxtheta,
                 dir*maxtheta/(ncp + 1))[c(-1, -(ncp + 2))]
    costheta <- cos(theta)
    sintheta <- sin(theta)
    # May have BOTH multiple end points AND multiple
    # control points to generate (per set of end points)
    # Generate consecutive sets of control points by performing
    # matrix multiplication
    cpx <- ox + ((newx1 - ox) %*% t(costheta)) -
        ((y1 - oy) %*% t(sintheta))
    cpy <- oy + ((y1 - oy) %*% t(costheta)) +
        ((newx1 - ox) %*% t(sintheta))

    # Reverse transformations (scaling and rotation) to
    # produce control points in the original space
    cpx <- cpx/scalex
    sinnb <- sin(-beta)
    cosnb <- cos(-beta)
    finalcpx <- x1 + (cpx - x1)*cosnb - (cpy - y1)*sinnb
    finalcpy <- y1 + (cpy - y1)*cosnb + (cpx - x1)*sinnb

    # Debugging
    if (debug) {
        ox <- ox/scalex
        fox <- x1 + (ox - x1)*cosnb - (oy - y1)*sinnb
        foy <- y1 + (oy - y1)*cosnb + (ox - x1)*sinnb
        grid.points(fox, foy, default.units="inches",
                    pch=16, size=unit(1, "mm"),
                    gp=gpar(col="grey"))
        grid.circle(fox, foy, sqrt((ox - x1)^2 + (oy - y1)^2),
                    default.units="inches",
                    gp=gpar(col="grey"))
    }

    list(x=as.numeric(t(finalcpx)), y=as.numeric(t(finalcpy)))
}

# Debugging
cbDiagram <- function(x1, y1, x2, y2, cps) {
    grid.segments(x1, y1, x2, y2,
                gp=gpar(col="grey"),
                default.units="inches")
    grid.points(x1, y1, pch=16, size=unit(1, "mm"),
                gp=gpar(col="green"),
                default.units="inches")
    grid.points(x2, y2, pch=16, size=unit(1, "mm"),
                gp=gpar(col="red"),
                default.units="inches")
    grid.points(cps$x, cps$y, pch=16, size=unit(1, "mm"),
                default.units="inches",
                gp=gpar(col="blue"))
}

straightCurve <- function(x1, y1, x2, y2, arrow, debug) {
    if (debug) {
        xm <- (x1 + x2)/2
        ym <- (y1 + y2)/2
        cbDiagram(x1, y1, x2, y2, list(x=xm, y=ym))
    }

    segmentsGrob(x1, y1, x2, y2,
                 default.units="inches",
                 arrow=arrow, name="segment")
}

# Return a gTree (even if it only has one grob as a child)
# because that is the only way to get more than one child
# to draw
calcCurveGrob <- function(x, debug) {
    x1 <- x$x1
    x2 <- x$x2
    y1 <- x$y1
    y2 <- x$y2
    curvature <- x$curvature
    angle <- x$angle
    ncp <- x$ncp
    shape <- x$shape
    square <- x$square
    squareShape <- x$squareShape
    inflect <- x$inflect
    arrow <- x$arrow
    open <- x$open

    # Calculate a set of control points based on:
    # 'curvature', ' angle', and 'ncp',
    # and the start and end point locations.

    # The origin is a point along the perpendicular bisector
    # of the line between the end points.

    # The control points are found by rotating the end points
    # about the origin.

    # Do everything in inches to make things easier.
    # Because this is within a makeContent() method,
    # the conversions will not be an
    # issue (in terms of device resizes).
    x1 <- convertX(x1, "inches", valueOnly=TRUE)
    y1 <- convertY(y1, "inches", valueOnly=TRUE)
    x2 <- convertX(x2, "inches", valueOnly=TRUE)
    y2 <- convertY(y2, "inches", valueOnly=TRUE)

    # Outlaw identical end points
    if (any(x1 == x2 & y1 == y2))
        stop("end points must not be identical")

    # Rep locations to allow multiple curves from single call
    maxn <- max(length(x1),
                length(y1),
                length(x2),
                length(y2))
    x1 <- rep(x1, length.out=maxn)
    y1 <- rep(y1, length.out=maxn)
    x2 <- rep(x2, length.out=maxn)
    y2 <- rep(y2, length.out=maxn)
    if (!is.null(arrow))
        arrow <- rep(arrow, length.out=maxn)

    if (curvature == 0) {
        children <- gList(straightCurve(x1, y1, x2, y2, arrow, debug))
    } else {
        # Treat any angle less than 1 or greater than 179 degrees
        # as a straight line
        # Takes care of some nasty limit effects as well as simplifying
        # things
        if (angle < 1 || angle > 179) {
            children <- gList(straightCurve(x1, y1, x2, y2, arrow, debug))
        } else {
            # Handle 'square' vertical and horizontal lines
            # separately
            if (square && any(x1 == x2 | y1 == y2)) {
                subset <- x1 == x2 | y1 == y2
                straightGrob <- straightCurve(x1[subset], y1[subset],
                                               x2[subset], y2[subset],
                                               arrow, debug)
                # Remove these from the curves to draw
                x1 <- x1[!subset]
                x2 <- x2[!subset]
                y1 <- y1[!subset]
                y2 <- y2[!subset]
                if (!is.null(arrow))
                    arrow <- arrow[!subset]
            } else {
                straightGrob <- NULL
            }
            ncurve <- length(x1)
            # If nothing to draw, we're done
            if (ncurve == 0) {
                children <- gList(straightGrob)
            } else {
                if (inflect) {
                    xm <- (x1 + x2)/2
                    ym <- (y1 + y2)/2
                    shape1 <- rep(rep(shape, length.out=ncp), ncurve)
                    shape2 <- rev(shape1)
                    if (square) {
                      # If 'square' then add an extra control point
                        cps1 <- calcSquareControlPoints(x1, y1, xm, ym,
                                                        curvature, angle,
                                                        ncp,
                                                        debug=debug)
                        cps2 <- calcSquareControlPoints(xm, ym, x2, y2,
                                                        -curvature, angle,
                                                        ncp,
                                                        debug=debug)
                        shape1 <- interleave(ncp, ncurve, shape1,
                                             squareShape, squareShape,
                                             cps1$end)
                        shape2 <- interleave(ncp, ncurve, shape2,
                                             squareShape, squareShape,
                                             cps2$end)
                        ncp <- ncp + 1
                    } else {
                        cps1 <- calcControlPoints(x1, y1, xm, ym,
                                                  curvature, angle, ncp,
                                                  debug=debug)
                        cps2 <- calcControlPoints(xm, ym, x2, y2,
                                                  -curvature, angle, ncp,
                                                  debug=debug)
                    }

                    if (debug) {
                        cbDiagram(x1, y1, xm, ym, cps1)
                        cbDiagram(xm, ym, x2, y2, cps2)
                    }

                    idset <- 1L:ncurve
                    splineGrob <-
                        xsplineGrob(c(x1, cps1$x, xm, cps2$x, x2),
                                    c(y1, cps1$y, ym, cps2$y, y2),
                                    id=c(idset, rep(idset, each=ncp),
                                      idset, rep(idset, each=ncp),
                                      idset),
                                    default.units="inches",
                                    shape=c(rep(0, ncurve), shape1,
                                      rep(0, ncurve), shape2,
                                      rep(0, ncurve)),
                                    arrow=arrow, open=open,
                                    name="xspline")
                    if (is.null(straightGrob)) {
                        children <- gList(splineGrob)
                    } else {
                        children <- gList(straightGrob, splineGrob)
                    }
                } else {
                    shape <- rep(rep(shape, length.out=ncp), ncurve)
                    if (square) {
                      # If 'square' then add an extra control point
                        cps <- calcSquareControlPoints(x1, y1, x2, y2,
                                                       curvature, angle,
                                                       ncp,
                                                       debug=debug)
                        shape <- interleave(ncp, ncurve, shape,
                                            squareShape, squareShape,
                                            cps$end)
                        ncp <- ncp + 1
                    } else {
                        cps <- calcControlPoints(x1, y1, x2, y2,
                                                 curvature, angle, ncp,
                                                 debug=debug)
                    }
                    if (debug) {
                        cbDiagram(x1, y1, x2, y2, cps)
                    }

                    idset <- 1L:ncurve
                    splineGrob <- xsplineGrob(c(x1, cps$x, x2),
                                              c(y1, cps$y, y2),
                                              id=c(idset,
                                                rep(idset, each=ncp), idset),
                                              default.units="inches",
                                              shape=c(rep(0, ncurve), shape,
                                                rep(0, ncurve)),
                                              arrow=arrow, open=open,
                                              name="xspline")
                    if (is.null(straightGrob)) {
                        children <- gList(splineGrob)
                    } else {
                        children <- gList(straightGrob, splineGrob)
                    }
                }
            }
        }
    }
    gTree(children=children,
          name=x$name, gp=x$gp, vp=x$vp)
}

validDetails.curve <- function(x) {
    if ((!is.unit(x$x1) || !is.unit(x$y1)) ||
        (!is.unit(x$x2) || !is.unit(x$y2)))
        stop("'x1', 'y1', 'x2', and 'y2' must be units")
    x$curvature <- as.numeric(x$curvature)
    x$angle <- x$angle %% 180
    x$ncp <- as.integer(x$ncp)
    if (x$shape < -1 || x$shape > 1)
        stop("'shape' must be between -1 and 1")
    x$square <- as.logical(x$square)
    if (x$squareShape < -1 || x$squareShape > 1)
        stop("'squareShape' must be between -1 and 1")
    x$inflect <- as.logical(x$inflect)
    if (!is.null(x$arrow) && !inherits(x$arrow, "arrow"))
        stop("'arrow' must be an arrow object or NULL")
    x$open <- as.logical(x$open)
    x
}

makeContent.curve <- function(x) {
    calcCurveGrob(x, x$debug)
}

xDetails.curve <- function(x, theta) {
    cg <- calcCurveGrob(x, FALSE)
    # Could do better here
    # (result for more than 1 child is basically to give up)
    if (length(cg$children) == 1)
        xDetails(cg$children[[1]], theta)
    else
        xDetails(cg, theta)
}

yDetails.curve <- function(x, theta) {
    cg <- calcCurveGrob(x, FALSE)
    if (length(cg$children) == 1)
        yDetails(cg$children[[1]], theta)
    else
        yDetails(cg, theta)
}

widthDetails.curve <- function(x) {
    cg <- calcCurveGrob(x, FALSE)
    if (length(cg$children) == 1)
        widthDetails(cg$children[[1]])
    else
        widthDetails(cg)
}

heightDetails.curve <- function(x) {
    cg <- calcCurveGrob(x, FALSE)
    if (length(cg$children) == 1)
        heightDetails(cg$children[[1]])
    else
        heightDetails(cg)
}

curveGrob <- function(x1, y1, x2, y2, default.units="npc",
                      curvature=1, angle=90, ncp=1,
                      shape=0.5, square=TRUE, squareShape=1,
                      inflect=FALSE, arrow=NULL, open=TRUE,
                      debug=FALSE,
                      name=NULL, gp=gpar(), vp=NULL) {
    # FIXME:  add arg checking
    # FIXME:  angle MUST be between 0 and 180
    if (!is.unit(x1))
        x1 <- unit(x1, default.units)
    if (!is.unit(y1))
        y1 <- unit(y1, default.units)
    if (!is.unit(x2))
        x2 <- unit(x2, default.units)
    if (!is.unit(y2))
        y2 <- unit(y2, default.units)
    gTree(x1=x1, y1=y1, x2=x2, y2=y2,
          curvature=curvature, angle=angle, ncp=ncp,
          shape=shape, square=square, squareShape=squareShape,
          inflect=inflect, arrow=arrow, open=open, debug=debug,
          name=name, gp=gp, vp=vp,
          cl="curve")
}

grid.curve <- function(...) {
    grid.draw(curveGrob(...))
}

# Calculate the curvature to use if you want to produce control
# points lying along the arc of a circle that spans theta degrees
# (Use ncp=8 and shape=-1 to actually produce such an arc)
arcCurvature <- function(theta) {
    # Avoid limiting cases (just draw a straight line)
    if (theta < 1 || theta > 359)
        return(0)
    angle <- 0.5*theta/180*pi
    1/sin(angle) - 1/tan(angle)
}

