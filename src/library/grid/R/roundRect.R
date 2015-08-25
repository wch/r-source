#  File src/library/grid/R/roundrect.R
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


# Good idea to choose r as absolute unit or "snpc"
roundrectGrob <- function(x=0.5, y=0.5, width=1, height=1,
                          default.units="npc",
                          r=unit(0.1, "snpc"),
                          just="centre",
                          name=NULL, gp=NULL, vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(width))
        width <- unit(width, default.units)
    if (!is.unit(height))
        height <- unit(height, default.units)
    grob(x=x, y=y, width=width, height=height, r=r, just=just,
         name=name, gp=gp, vp=vp, cl="roundrect")
}

grid.roundrect <- function(...) {
  grid.draw(roundrectGrob(...))
}

validDetails.roundrect <- function(x) {
    if (!is.unit(x$x) ||
        !is.unit(x$y) ||
        !is.unit(x$width) ||
        !is.unit(x$height))
        stop("'x', 'y', 'width', and 'height' must be units")
    if (!is.unit(x$r))
        stop("'r' must be a 'unit' object")
    valid.just(x$just)
    # Make sure that x and y are of length 1
    if (length(x$x) != 1 | length(x$y) != 1 |
        length(x$width) != 1 | length(x$height) != 1)
        stop("'x', 'y', 'width', and 'height' must have length 1")
    x
}

makeContext.roundrect <- function(x) {
    rrvp <- viewport(x$x, x$y, x$width, x$height, just=x$just,
                     name="rrvp")
    if (!is.null(x$vp)) {
        x$vp <- vpStack(x$vp, rrvp)
    } else {
        x$vp <- rrvp
    }
    x
}

# x, y, is the real corner
roundCorner <- function(num, x, y, r) {
  n <- 10*4
  t <- seq(0, 2*pi, length.out=n)
  cost <- cos(t)
  sint <- sin(t)
  if (num == 1) {
    xc <- x + r
    yc <- y + r
    subset <- (n/2):(3*n/4)
  } else if (num == 2) {
    xc <- x + r
    yc <- y - r
    subset <- (n/4):(n/2)
  } else if (num == 3) {
    xc <- x - r
    yc <- y - r
    subset <- 1L:(n/4)
  } else if (num == 4) {
    xc <- x - r
    yc <- y + r
    subset <- (3*n/4):n
  }
  list(x=xc + (cost*r)[subset], y=yc + (sint*r)[subset])
}

rrpoints <- function(x) {
  left <- 0
  bottom <- 0
  right <- convertX(unit(1, "npc"), "inches", valueOnly=TRUE)
  top <- convertY(unit(1, "npc"), "inches", valueOnly=TRUE)
  r <- min(convertWidth(x$r, "inches", valueOnly=TRUE),
           convertHeight(x$r, "inches", valueOnly=TRUE))
  corner1 <- roundCorner(1, left, bottom, r)
  corner2 <- roundCorner(2, left, top, r)
  corner3 <- roundCorner(3, right, top, r)
  corner4 <- roundCorner(4, right, bottom, r)
  xx <- unit(c(left + r, right - r, corner4$x,
               right, right, corner3$x,
               right - r, left + r, corner2$x,
               left, left, corner1$x),
             "inches")
  yy <- unit(c(bottom, bottom, corner4$y,
               bottom + r, top - r, corner3$y,
               top, top, corner2$y,
               top - r, bottom + r, corner1$y),
             "inches")
  list(x=xx, y=yy)
}

makeContent.roundrect <- function(x) {
    boundary <- rrpoints(x)
    polygonGrob(boundary$x, boundary$y,
                name=x$name, gp=x$gp, vp=x$vp)
}

xDetails.roundrect <- function(x, theta) {
    boundary <- rrpoints(x)
    bounds <- grid.Call(L_locnBounds, boundary$x, boundary$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.roundrect <- function(x, theta) {
    boundary <- rrpoints(x)
    bounds <- grid.Call(L_locnBounds, boundary$x, boundary$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.roundrect <- function(x) {
    boundary <- rrpoints(x)
    bounds <- grid.Call(L_locnBounds, boundary$x, boundary$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[3L], "inches")
}

heightDetails.roundrect <- function(x) {
    boundary <- rrpoints(x)
    bounds <- grid.Call(L_locnBounds, boundary$x, boundary$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[4L], "inches")
}



