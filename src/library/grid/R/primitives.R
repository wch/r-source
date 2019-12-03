#  File src/library/grid/R/primitives.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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


# Function that creates a description of an arrow head
# to add to a line
arrow <- function(angle=30, length=unit(0.25, "inches"),
                  ends="last", type="open") {
    angle <- as.numeric(angle)
    if (!is.unit(length))
        stop("'length' must be a 'unit' object")
    ends <- as.integer(match(ends, c("first", "last", "both")))
    type <- as.integer(match(type, c("open", "closed")))
    if (anyNA(ends) || anyNA(type) ||
        length(ends) == 0 || length(type) == 0)
        stop("invalid 'ends' or 'type' argument")
    a <- list(angle=angle, length=length,
              ends=ends, type=type)
    class(a) <- "arrow"
    a
}

length.arrow <- function(x) {
    max(do.call("max", lapply(x, length)),
                length(x$length))
}

rep.arrow <- function(x, ...) {
    maxn <- length(x)
    newa <- list(angle=rep(x$angle, length.out=maxn),
                 length=rep(x$length, length.out=maxn),
                 ends=rep(x$ends, length.out=maxn),
                 type=rep(x$type, length.out=maxn))
    newa <- lapply(newa, rep, ...)
    class(newa) <- "arrow"
    newa
}

# Method for subsetting "arrow" objects
`[.arrow` <- function(x, index, ...) {
    if (length(index) == 0)
        return(NULL)
    maxn <- length(x)
    newa <- list(angle=rep(x$angle, length.out=maxn),
                 length=rep(x$length, length.out=maxn),
                 ends=rep(x$ends, length.out=maxn),
                 type=rep(x$type, length.out=maxn))
    newa <- lapply(X = newa, FUN = "[", index, ...)
    class(newa) <- "arrow"
    newa
}

str.arrow <- function(object, ...) {
  o <- oldClass(object)
  oldClass(object) <- setdiff(o, "arrow")
  str(object)
}

######################################
# move-to and line-to primitives
######################################
validDetails.move.to <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  # Make sure that x and y are of length 1
  if (length(x$x) > 1 | length(x$y) > 1)
    stop("'x' and 'y' must have length 1")
  x
}

drawDetails.move.to <- function(x, recording=TRUE) {
  grid.Call.graphics(C_moveTo, x$x, x$y)
}

moveToGrob <- function(x=0, y=0,
                       default.units="npc",
                       name=NULL, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y,
       name=name, vp=vp, cl="move.to")
}

grid.move.to <- function(x=0, y=0,
                         default.units="npc",
                         name=NULL, draw=TRUE, vp=NULL) {
  mtg <- moveToGrob(x=x, y=y, default.units=default.units,
                    name=name, vp=vp)
  if (draw)
    grid.draw(mtg)
  invisible(mtg)
}

validDetails.line.to <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  # Make sure that x and y are of length 1
  if (length(x$x) > 1 | length(x$y) > 1)
    stop("'x' and 'y' must have length 1")
  if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
      stop("invalid 'arrow' argument")
  x
}

drawDetails.line.to <- function(x, recording=TRUE) {
  grid.Call.graphics(C_lineTo, x$x, x$y, x$arrow)
}

lineToGrob <- function(x=1, y=1,
                       default.units="npc",
                       arrow=NULL,
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, arrow=arrow,
       name=name, gp=gp, vp=vp, cl="line.to")
}

grid.line.to <- function(x=1, y=1,
                         default.units="npc",
                         arrow=NULL,
                         name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  ltg <- lineToGrob(x=x, y=y, default.units=default.units, arrow=arrow,
                    name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(ltg)
  invisible(ltg)
}

######################################
# LINES primitive
######################################
validDetails.lines <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
      stop("invalid 'arrow' argument")
  x
}

drawDetails.lines <- function(x, recording=TRUE) {
    grid.Call.graphics(C_lines, x$x, x$y,
                       list(as.integer(1L:max(length(x$x), length(x$y)))),
                       x$arrow)
}

xDetails.lines <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.lines <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.lines <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.lines <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

linesGrob <- function(x=unit(c(0, 1), "npc"),
                      y=unit(c(0, 1), "npc"),
                      default.units="npc",
                      arrow=NULL,
                      name=NULL, gp=gpar(), vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y,
       arrow=arrow, name=name, gp=gp, vp=vp, cl="lines")
}

grid.lines <- function(x=unit(c(0, 1), "npc"),
                       y=unit(c(0, 1), "npc"),
                       default.units="npc",
                       arrow=NULL,
                       name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  lg <- linesGrob(x=x, y=y,
                  default.units=default.units, arrow=arrow,
                  name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(lg)
  invisible(lg)
}

######################################
# POLYLINES primitive
######################################
# Very similar to LINES primitive, but allows
# multiple polylines via 'id' and 'id.lengths' args
# as per POLYGON primitive
validDetails.polyline <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
      stop("'x' and 'y' must be units")
  if (!is.null(x$id) && !is.null(x$id.lengths))
      stop("it is invalid to specify both 'id' and 'id.lengths'")
  if (length(x$x) != length(x$y))
      stop("'x' and 'y' must be same length")
  if (!is.null(x$id) && (length(x$id) != length(x$x)))
      stop("'x' and 'y' and 'id' must all be same length")
  if (!is.null(x$id))
      x$id <- as.integer(x$id)
  if (!is.null(x$id.lengths) && (sum(x$id.lengths) != length(x$x)))
      stop("'x' and 'y' and 'id.lengths' must specify same overall length")
  if (!is.null(x$id.lengths))
      x$id.lengths <- as.integer(x$id.lengths)
  if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
      stop("invalid 'arrow' argument")
  x
}

drawDetails.polyline <- function(x, recording=TRUE) {
    if (is.null(x$id) && is.null(x$id.lengths))
        grid.Call.graphics(C_lines, x$x, x$y,
                           list(as.integer(seq_along(x$x))),
                           x$arrow)
    else {
        if (is.null(x$id)) {
            n <- length(x$id.lengths)
            id <- rep(1L:n, x$id.lengths)
        } else {
            n <- length(unique(x$id))
            id <- x$id
        }
        index <- split(as.integer(seq_along(x$x)), id)
        grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow)
    }
}

xDetails.polyline <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.polyline <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.polyline <- function(x) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[3L], "inches")
}

heightDetails.polyline <- function(x) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[4L], "inches")
}

polylineGrob <- function(x=unit(c(0, 1), "npc"),
                         y=unit(c(0, 1), "npc"),
                         id=NULL, id.lengths=NULL,
                         default.units="npc",
                         arrow=NULL,
                         name=NULL, gp=gpar(), vp=NULL) {
    # Allow user to specify unitless vector;  add default units
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(x=x, y=y, id=id, id.lengths=id.lengths,
         arrow=arrow, name=name, gp=gp, vp=vp, cl="polyline")
}

grid.polyline <- function(...) {
    grid.draw(polylineGrob(...))
}

######################################
# SEGMENTS primitive
######################################
validDetails.segments <- function(x) {
  if (!is.unit(x$x0) || !is.unit(x$x1) ||
      !is.unit(x$y0) || !is.unit(x$y1))
    stop("'x0', 'y0', 'x1', and 'y1' must be units")
  if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
      stop("invalid 'arrow' argument")
  x
}

drawDetails.segments <- function(x, recording=TRUE) {
  grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow)
}

segmentBounds <- function(x, theta) {
    n <- max(length(x$x0), length(x$x1),
             length(x$y0), length(x$y1))
    x0 <- rep(x$x0, length.out=n)
    x1 <- rep(x$x1, length.out=n)
    y0 <- rep(x$y0, length.out=n)
    y1 <- rep(x$y1, length.out=n)
    grid.Call(C_locnBounds, unit.c(x0, x1), unit.c(y0, y1), theta)
}

xDetails.segments <- function(x, theta) {
    bounds <- segmentBounds(x, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.segments <- function(x, theta) {
    bounds <- segmentBounds(x, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.segments <- function(x) {
    bounds <- segmentBounds(x, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[3L], "inches")
}

heightDetails.segments <- function(x) {
    bounds <- segmentBounds(x, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[4L], "inches")
}

segmentsGrob <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                         x1=unit(1, "npc"), y1=unit(1, "npc"),
                         default.units="npc",
                         arrow=NULL,
                         name=NULL, gp=gpar(), vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x0))
    x0 <- unit(x0, default.units)
  if (!is.unit(x1))
    x1 <- unit(x1, default.units)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units)
  if (!is.unit(y1))
    y1 <- unit(y1, default.units)
  grob(x0=x0, y0=y0, x1=x1, y1=y1, arrow=arrow, name=name, gp=gp, vp=vp,
       cl="segments")
}

grid.segments <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                          x1=unit(1, "npc"), y1=unit(1, "npc"),
                          default.units="npc",
                          arrow=NULL,
                          name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  sg <- segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                     default.units=default.units,
                     arrow=arrow,
                     name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(sg)
  invisible(sg)
}

######################################
# ARROWS primitive
######################################

# Superceded by 'arrow' arg to line-drawing primitives
# which contains an "arrow" object
validDetails.arrows <- function(x) {
  if ((!is.null(x$x) && !is.unit(x$x)) ||
      (!is.null(x$y) && !is.unit(x$y)))
    stop("'x' and 'y' must be units or NULL")
  if (!is.unit(x$length))
    stop("'length' must be a 'unit' object")
  x$ends <- as.integer(match(x$ends, c("first", "last", "both")))
  x$type <- as.integer(match(x$type, c("open", "closed")))
  if (any(is.na(x$ends)) || any(is.na(x$type)))
    stop("invalid 'ends' or 'type' argument")
  x
}

drawDetails.arrows <- function(x, recording=TRUE) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    # This could be done via method dispatch, but that really
    # seemed like overkill
    # OTOH, this is NOT user-extensible
    # AND the code for, e.g., "lines" is not located with
    # the other grid.lines code so changes there are unlikely
    # to propagate to here (e.g., add an id arg to grid.lines?
    if (inherits(lineThing, "line.to")) {
      x1 <- NULL
      x2 <- lineThing$x
      y1 <- NULL
      y2 <- lineThing$y
      xnm1 <- NULL
      xn <- lineThing$x
      ynm1 <- NULL
      yn <- lineThing$y
    } else if (inherits(lineThing, "lines")) {
      # x or y may be recycled
      n <- max(length(lineThing$x),
               length(lineThing$y))
      xx <- rep(lineThing$x, length.out=2)
      x1 <- xx[1L]
      x2 <- xx[2L]
      xx <- rep(lineThing$x, length.out=n)
      xnm1 <- xx[n - 1]
      xn <- xx[n]
      yy <- rep(lineThing$y, length.out=2)
      y1 <- yy[1L]
      y2 <- yy[2L]
      yy <- rep(lineThing$y, length.out=n)
      ynm1 <- yy[n - 1]
      yn <- yy[n]
    } else { # inherits(lineThing, "segments")
      x1 <- lineThing$x0
      x2 <- lineThing$x1
      xnm1 <- lineThing$x0
      xn <- lineThing$x1
      y1 <- lineThing$y0
      y2 <- lineThing$y1
      ynm1 <- lineThing$y0
      yn <- lineThing$y1
    }
  } else {
    # x or y may be recycled
    n <- max(length(x$x), length(x$y))
    xx <- rep(x$x, length.out=2)
    x1 <- xx[1L]
    x2 <- xx[2L]
    xx <- rep(x$x, length.out=n)
    xnm1 <- xx[n - 1]
    xn <- xx[n]
    yy <- rep(x$y, length.out=2)
    y1 <- yy[1L]
    y2 <- yy[2L]
    yy <- rep(x$y, length.out=n)
    ynm1 <- yy[n - 1]
    yn <- yy[n]
    grid.Call.graphics(C_lines, x$x, x$y,
                       list(as.integer(1L:n)),
                       NULL)
  }
  grid.Call.graphics(C_arrows, x1, x2, xnm1, xn, y1, y2, ynm1, yn,
                     x$angle, x$length, x$ends, x$type)
}

widthDetails.arrows <- function(x) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    widthDetails(lineThing)
  } else {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
      unit(0, "inches")
    else
      unit(bounds[3L], "inches")
  }
}

heightDetails.arrows <- function(x) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    heightDetails(lineThing)
  } else {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
      unit(0, "inches")
    else
      unit(bounds[4L], "inches")
  }
}

arrowsGrob <- function(x=c(0.25, 0.75), y=0.5,
                       default.units="npc",
                       grob=NULL,
                       angle=30, length=unit(0.25, "inches"),
                       ends="last", type="open",
                       name=NULL, gp=gpar(), vp=NULL) {
    .Defunct(msg="'arrowsGrob' is defunct; use 'arrow' arguments to line drawing functions")
}

grid.arrows <- function(x=c(0.25, 0.75), y=0.5,
                        default.units="npc",
                        grob=NULL,
                        angle=30, length=unit(0.25, "inches"),
                        ends="last", type="open",
                        name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
    .Defunct(msg="'grid.arrows' is defunct; use 'arrow' arguments to line drawing functions")
}

######################################
# POLYGON primitive
######################################

validDetails.polygon <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  if (!is.null(x$id) && !is.null(x$id.lengths))
    stop("it is invalid to specify both 'id' and 'id.lengths'")
  if (length(x$x) != length(x$y))
    stop("'x' and 'y' must be same length")
  if (!is.null(x$id) && (length(x$id) != length(x$x)))
    stop("'x' and 'y' and 'id' must all be same length")
  if (!is.null(x$id))
    x$id <- as.integer(x$id)
  if (!is.null(x$id.lengths) && (sum(x$id.lengths) != length(x$x)))
    stop("'x' and 'y' and 'id.lengths' must specify same overall length")
  if (!is.null(x$id.lengths))
    x$id.lengths <- as.integer(x$id.lengths)
  x
}

drawDetails.polygon <- function(x, recording=TRUE) {
  if (is.null(x$id) && is.null(x$id.lengths))
    grid.Call.graphics(C_polygon, x$x, x$y,
                       list(as.integer(seq_along(x$x))))
  else {
    if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
    } else {
      n <- length(unique(x$id))
      id <- x$id
    }
    index <- split(as.integer(seq_along(x$x)), id)
    grid.Call.graphics(C_polygon, x$x, x$y, index)
  }
}

xDetails.polygon <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.polygon <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.polygon <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.polygon <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

polygonGrob <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                        id=NULL, id.lengths=NULL,
                        default.units="npc",
                        name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, id=id,
       id.lengths=id.lengths,
       name=name, gp=gp, vp=vp, cl="polygon")
}

grid.polygon <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                         id=NULL, id.lengths=NULL,
                         default.units="npc",
                         name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  pg <- polygonGrob(x=x, y=y, id=id, id.lengths=id.lengths,
                    default.units=default.units,
                    name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(pg)
  invisible(pg)
}

######################################
# PATH primitive
######################################

validDetails.pathgrob <- function(x) {
    if (!is.unit(x$x) || !is.unit(x$y))
        stop("'x' and 'y' must be units")
    if (!is.null(x$id) && !is.null(x$id.lengths))
        stop("it is invalid to specify both 'id' and 'id.lengths'")
    if (length(x$x) != length(x$y))
        stop("'x' and 'y' must be same length")
    if (!is.null(x$id) && (length(x$id) != length(x$x)))
        stop("'x' and 'y' and 'id' must all be same length")
    if (!is.null(x$id))
        x$id <- as.integer(x$id)
    if (!is.null(x$pathId))
    	x$pathId <- as.integer(x$pathId)
    if (!is.null(x$id.lengths) && (sum(x$id.lengths) != length(x$x)))
        stop("'x' and 'y' and 'id.lengths' must specify same overall length")
    if (!is.null(x$pathId.lengths) && (sum(x$pathId.lengths) != length(x$x)))
    	stop("'x' and 'y' and 'pathId.lengths' must specify same overall length")
    if (!is.null(x$id.lengths))
        x$id.lengths <- as.integer(x$id.lengths)
    if (!is.null(x$pathId.lengths))
    	x$pathId.lengths <- as.integer(x$pathId.lengths)
    x
}

xDetails.pathgrob <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.pathgrob <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.pathgrob <- function(x) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[3L], "inches")
}

heightDetails.pathgrob <- function(x) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[4L], "inches")
}


drawDetails.pathgrob <- function(x, recording=TRUE) {
    hasMultiple <- !(is.null(x$pathId) && is.null(x$pathId.lengths))
    if (hasMultiple) {
        if (is.null(x$pathId)) {
            n <- length(x$pathId.lengths)
            pathId <- rep(1L:n, x$pathId.lengths)
        } else {
            pathId <- x$pathId
        }
    }
    if (is.null(x$id) && is.null(x$id.lengths)) {
        if (hasMultiple) {
            grid.Call.graphics(C_polygon, x$x, x$y,
                               split(as.integer(seq_along(x$x)), pathId))
        } else {
            grid.Call.graphics(C_polygon, x$x, x$y,
                               list(as.integer(seq_along(x$x))))
        }
    } else {
        if (is.null(x$id)) {
            n <- length(x$id.lengths)
            id <- rep(1L:n, x$id.lengths)
        } else {
            n <- length(unique(x$id))
            id <- x$id
        }
        if (hasMultiple) {
            index <- mapply(split,
                            x=split(as.integer(seq_along(x$x)), pathId), 
                            f=split(id, pathId),
                            SIMPLIFY = FALSE, USE.NAMES = FALSE)
        } else {
            index <- list(split(as.integer(seq_along(x$x)), id))
        }
        grid.Call.graphics(C_path, x$x, x$y, index,
                           switch(x$rule, winding=1L, evenodd=0L))
    }
}

pathGrob <- function(x, y,
                     id=NULL, id.lengths=NULL,
                     pathId=NULL, pathId.lengths=NULL,
                     rule="winding",
                     default.units="npc",
                     name=NULL, gp=gpar(), vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(x=x, y=y, id=id, id.lengths=id.lengths,
         pathId=pathId, pathId.lengths=pathId.lengths,
         rule=rule,
         name=name, gp=gp, vp=vp, cl="pathgrob")
}

grid.path <- function(...) {
  grid.draw(pathGrob(...))
}

######################################
# XSPLINE primitive
######################################

validDetails.xspline <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("x and y must be units")
  if (!is.null(x$id) && !is.null(x$id.lengths))
    stop("it is invalid to specify both 'id' and 'id.lengths'")
  nx <- length(x$x)
  ny <- length(x$y)
  if (nx != ny)
    stop("'x' and 'y' must be same length")
  if (!is.null(x$id) && (length(x$id) != nx))
    stop("'x' and 'y' and 'id' must all be same length")
  if (!is.null(x$id))
    x$id <- as.integer(x$id)
  if (!is.null(x$id.lengths) && (sum(x$id.lengths) != nx))
    stop("'x' and 'y' and 'id.lengths' must specify same overall length")
  if (!is.null(x$id.lengths))
    x$id.lengths <- as.integer(x$id.lengths)
  if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
      stop("invalid 'arrow' argument")
  if (any(x$shape < -1 | x$shape > 1))
    stop("'shape' must be between -1 and 1")
  x$open <- as.logical(x$open)
  # Force all first and last shapes to be 0 for open xsplines
  if (x$open) {
      x$shape <- rep(x$shape, length.out=nx)
      # Watch out for id or id.length!
      index <- xsplineIndex(x)
      first <- sapply(index, min)
      last <- sapply(index, max)
      x$shape[c(first, last)] <- 0
  }
  x
}

xsplineIndex <- function(x) {
  if (is.null(x$id) && is.null(x$id.lengths))
      list(as.integer(seq_along(x$x)))
  else {
    if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1L:n, x$id.lengths)
    } else {
      n <- length(unique(x$id))
      id <- x$id
    }
    split(as.integer(seq_along(x$x)), id)
  }
}

drawDetails.xspline <- function(x, recording=TRUE) {
    grid.Call.graphics(C_xspline, x$x, x$y, x$shape, x$open, x$arrow,
                       x$repEnds, xsplineIndex(x))
}

xDetails.xspline <- function(x, theta) {
  bounds <- grid.Call(C_xsplineBounds, x$x, x$y, x$shape, x$open, x$arrow,
                      x$repEnds, xsplineIndex(x), theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[1L], "inches")
}

yDetails.xspline <- function(x, theta) {
  bounds <- grid.Call(C_xsplineBounds, x$x, x$y, x$shape, x$open, x$arrow,
                      x$repEnds, xsplineIndex(x), theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[2L], "inches")
}

widthDetails.xspline <- function(x) {
  bounds <- grid.Call(C_xsplineBounds, x$x, x$y, x$shape, x$open, x$arrow,
                      x$repEnds, list(as.integer(seq_along(x$x))), 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.xspline <- function(x) {
  bounds <- grid.Call(C_xsplineBounds, x$x, x$y, x$shape, x$open, x$arrow,
                      x$repEnds, list(as.integer(seq_along(x$x))), 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

xsplineGrob <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                        id=NULL, id.lengths=NULL,
                        default.units="npc",
                        shape=0, open=TRUE, arrow=NULL, repEnds=TRUE,
                        name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, shape=shape, open=open,
       id=id, id.lengths=id.lengths, arrow=arrow, repEnds=repEnds,
       name=name, gp=gp, vp=vp, cl="xspline")
}

grid.xspline <- function(...) {
  grid.draw(xsplineGrob(...))
}

xsplinePoints <- function(x) {
    # Mimic drawGrob() to ensure x$vp and x$gp enforced
    dlon <- grid.Call(C_setDLon, FALSE)
    on.exit(grid.Call(C_setDLon, dlon))
    tempgpar <- grid.Call(C_getGPar)
    on.exit(grid.Call(C_setGPar, tempgpar), add=TRUE)
    preDraw(x)
    # Raw pts in dev coords
    devPoints <- grid.Call(C_xsplinePoints,
                           x$x, x$y, x$shape, x$open, x$arrow,
                           x$repEnds, xsplineIndex(x), 0)
    postDraw(x)
    # Convert to units in inches
    unitPoints <- lapply(devPoints,
                         function(x) {
                             names(x) <- c("x", "y")
                             x$x <- unit(x$x, "inches")
                             x$y <- unit(x$y, "inches")
                             x
                         })
    if (length(unitPoints) == 1)
        unitPoints <- unitPoints[[1]]
    unitPoints
}

######################################
# BEZIER primitive
######################################

# A bezier grob that works of a (not-100% accurate) approximation
# using X-splines

# X-Spline approx to Bezier
Ms <- 1/6*rbind(c(1, 4, 1, 0),
                c(-3, 0, 3, 0),
                c(3, -6, 3, 0),
                c(-1, 3, -3, 1))
Msinv <- solve(Ms)
# Bezier control matrix
Mb <- rbind(c(1, 0, 0, 0),
            c(-3, 3, 0, 0),
            c(3, -6, 3, 0),
            c(-1, 3, -3, 1))

splinePoints <- function(xb, yb, idIndex) {
    xs <- unlist(lapply(idIndex,
                        function(i) {
                            Msinv %*% Mb %*% xb[i]
                        }))
    ys <- unlist(lapply(idIndex,
                        function(i) {
                            Msinv %*% Mb %*% yb[i]
                        }))
    list(x=xs, y=ys)
}

splinegrob <- function(x) {
    xx <- convertX(x$x, "inches", valueOnly=TRUE)
    yy <- convertY(x$y, "inches", valueOnly=TRUE)
    sp <- splinePoints(xx, yy, xsplineIndex(x))
    xsplineGrob(sp$x, sp$y, default.units="inches",
                id=x$id, id.lengths=x$id.lengths,
                shape=1, repEnds=FALSE,
                arrow=x$arrow, name=x$name,
                gp=x$gp, vp=x$vp)
}

validDetails.beziergrob <- function(x) {
    if (!is.unit(x$x) ||
        !is.unit(x$y))
        stop("x and y must be units")
    if (!is.null(x$id) && !is.null(x$id.lengths))
        stop("it is invalid to specify both 'id' and 'id.lengths'")
    nx <- length(x$x)
    ny <- length(x$y)
    if (nx != ny)
        stop("'x' and 'y' must be same length")
    if (!is.null(x$id) && (length(x$id) != nx))
        stop("'x' and 'y' and 'id' must all be same length")
    if (!is.null(x$id))
        x$id <- as.integer(x$id)
    if (!is.null(x$id.lengths) && (sum(x$id.lengths) != nx))
        stop("'x' and 'y' and 'id.lengths' must specify same overall length")
    if (!is.null(x$id.lengths))
        x$id.lengths <- as.integer(x$id.lengths)
    if (is.null(x$id) && is.null(x$id.lengths)) {
        if (length(x$x) != 4L)
            stop("must have exactly 4 control points")
    } else {
        if (is.null(x$id)) {
            n <- length(x$id.lengths)
            id <- rep(1L:n, x$id.lengths)
        } else {
            id <- x$id
        }
        xper <- split(x$x, id)
        if (any(lengths(xper) != 4L))
            stop("must have exactly 4 control points per Bezier curve")
    }
    if (!(is.null(x$arrow) || inherits(x$arrow, "arrow")))
        stop("invalid 'arrow' argument")
    x
}

makeContent.beziergrob <- function(x) {
    splinegrob(x)
}

xDetails.beziergrob <- function(x, theta) {
    xDetails(splinegrob(x), theta)
}

yDetails.beziergrob <- function(x, theta) {
    yDetails(splinegrob(x), theta)
}

widthDetails.beziergrob <- function(x) {
    widthDetails(splinegrob(x))
}

heightDetails.beziergrob <- function(x) {
    heightDetails(splinegrob(x))
}

bezierGrob <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                       id=NULL, id.lengths=NULL,
                       default.units="npc", arrow=NULL,
                       name=NULL, gp=gpar(), vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(x=x, y=y,
         id=id, id.lengths=id.lengths, arrow=arrow,
         name=name, gp=gp, vp=vp, cl="beziergrob")
}

grid.bezier <- function(...) {
    grid.draw(bezierGrob(...))
}

bezierPoints <- function(x) {
    sg <- splinegrob(x)
    # splinegrob() does not make use of x$vp
    sg$vp <- x$vp
    xsplinePoints(sg)
}


######################################
# CIRCLE primitive
######################################

validDetails.circle <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$r))
    stop("'x', 'y', and 'r' must be units")
  x
}

drawDetails.circle <- function(x, recording=TRUE) {
  grid.Call.graphics(C_circle, x$x, x$y, x$r)
}

xDetails.circle <- function(x, theta) {
  bounds <- grid.Call(C_circleBounds, x$x, x$y, x$r, theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[1L], "inches")
}

yDetails.circle <- function(x, theta) {
  bounds <- grid.Call(C_circleBounds, x$x, x$y, x$r, theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[2L], "inches")
}

widthDetails.circle <- function(x) {
  bounds <- grid.Call(C_circleBounds, x$x, x$y, x$r, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.circle <- function(x) {
  bounds <- grid.Call(C_circleBounds, x$x, x$y, x$r, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

circleGrob <- function(x=0.5, y=0.5, r=0.5,
                       default.units="npc",
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(r))
    r <- unit(r, default.units)
  grob(x=x, y=y, r=r, name=name, gp=gp, vp=vp, cl="circle")
}

grid.circle <- function(x=0.5, y=0.5, r=0.5,
                        default.units="npc",
                        name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  cg <- circleGrob(x=x, y=y, r=r,
                   default.units=default.units,
                   name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(cg)
  invisible(cg)
}

######################################
# RECT primitive
######################################
validDetails.rect <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$width) ||
      !is.unit(x$height))
    stop("'x', 'y', 'width', and 'height' must be units")
  valid.just(x$just)
  if (!is.null(x$hjust))
    x$hjust <- as.numeric(x$hjust)
  if (!is.null(x$vjust))
    x$vjust <- as.numeric(x$vjust)
  x
}

drawDetails.rect <- function(x, recording=TRUE) {
    grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
                       resolveHJust(x$just, x$hjust),
                       resolveVJust(x$just, x$vjust))
}

xDetails.rect <- function(x, theta) {
  bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[1L], "inches")
}

yDetails.rect <- function(x, theta) {
  bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[2L], "inches")
}

widthDetails.rect <- function(x) {
  bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.rect <- function(x) {
  bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

rectGrob <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     width=unit(1, "npc"), height=unit(1, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     default.units="npc",
                     name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  grob(x=x, y=y, width=width, height=height, just=just,
       hjust=hjust, vjust=vjust,
       name=name, gp=gp, vp=vp, cl="rect")
}

grid.rect <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      width=unit(1, "npc"), height=unit(1, "npc"),
                      just="centre", hjust=NULL, vjust=NULL,
                      default.units="npc",
                      name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  rg <- rectGrob(x=x, y=y, width=width, height=height, just=just,
                 hjust=hjust, vjust=vjust,
                 default.units=default.units,
                 name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(rg)
  invisible(rg)
}

######################################
# RASTER primitive
######################################

validDetails.rastergrob <- function(x) {
    if (!(is.raster(x$raster) || inherits(x$raster, "nativeRaster")))
        x$raster <- as.raster(x$raster)
    if (!is.unit(x$x) ||
        !is.unit(x$y) ||
        (!is.null(x$width) && !is.unit(x$width)) ||
        (!is.null(x$height) && !is.unit(x$height)))
        stop("'x', 'y', 'width', and 'height' must be units")
    valid.just(x$just)
    if (!is.null(x$hjust))
        x$hjust <- as.numeric(x$hjust)
    if (!is.null(x$vjust))
        x$vjust <- as.numeric(x$vjust)
    x
}

resolveRasterSize <- function(x) {
    if (is.null(x$width)) {
        if (is.null(x$height)) {
            rasterRatio <- dim(x$raster)[1]/dim(x$raster)[2]
            vpWidth <- convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
            vpHeight <- convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE)
            vpRatio <- vpHeight/vpWidth
            if (rasterRatio > vpRatio) {
                x$height <- unit(vpHeight, "inches")
                x$width <- unit(vpHeight*dim(x$raster)[2]/dim(x$raster)[1],
                                "inches")
            } else {
                x$width <- unit(vpWidth, "inches")
                x$height <- unit(vpWidth*dim(x$raster)[1]/dim(x$raster)[2],
                                 "inches")
            }
        } else {
            h <- convertHeight(x$height, "inches", valueOnly=TRUE)
            x$width <- unit(h*dim(x$raster)[2]/dim(x$raster)[1],
                            "inches")
        }
    } else {
        if (is.null(x$height)) {
            w <- convertWidth(x$width, "inches", valueOnly=TRUE)
            x$height <- unit(w*dim(x$raster)[1]/dim(x$raster)[2],
                             "inches")
        }
    }
    x
}

drawDetails.rastergrob <- function(x, recording=TRUE) {
    # At this point resolve NULL width/height based on
    # image dimensions
    x <- resolveRasterSize(x)
    if (is.null(x$width)) {
        if (is.null(x$height)) {
            rasterRatio <- dim(x$raster)[1]/dim(x$raster)[2]
            vpWidth <- convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
            vpHeight <- convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE)
            vpRatio <- vpHeight/vpWidth
            if (rasterRatio > vpRatio) {
                x$height <- unit(vpHeight, "inches")
                x$width <- unit(vpHeight*dim(x$raster)[2]/dim(x$raster)[1],
                                "inches")
            } else {
                x$width <- unit(vpWidth, "inches")
                x$height <- unit(vpWidth*dim(x$raster)[1]/dim(x$raster)[2],
                                 "inches")
            }
        } else {
            h <- convertHeight(x$height, "inches", valueOnly=TRUE)
            x$width <- unit(h*dim(x$raster)[2]/dim(x$raster)[1],
                            "inches")
        }
    } else {
        if (is.null(x$height)) {
            w <- convertWidth(x$width, "inches", valueOnly=TRUE)
            x$height <- unit(w*dim(x$raster)[1]/dim(x$raster)[2],
                             "inches")
        }
    }
    grid.Call.graphics(C_raster, x$raster,
                       x$x, x$y, x$width, x$height,
                       resolveHJust(x$just, x$hjust),
                       resolveVJust(x$just, x$vjust),
                       x$interpolate)
}

xDetails.rastergrob <- function(x, theta) {
    x <- resolveRasterSize(x)
    bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                        resolveHJust(x$just, x$hjust),
                        resolveVJust(x$just, x$vjust),
                        theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.rastergrob <- function(x, theta) {
    x <- resolveRasterSize(x)
    bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                        resolveHJust(x$just, x$hjust),
                        resolveVJust(x$just, x$vjust),
                        theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.rastergrob <- function(x) {
    x <- resolveRasterSize(x)
    bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                        resolveHJust(x$just, x$hjust),
                        resolveVJust(x$just, x$vjust),
                        0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[3L], "inches")
}

heightDetails.rastergrob <- function(x) {
    x <- resolveRasterSize(x)
    bounds <- grid.Call(C_rectBounds, x$x, x$y, x$width, x$height,
                        resolveHJust(x$just, x$hjust),
                        resolveVJust(x$just, x$vjust),
                        0)
    if (is.null(bounds))
        unit(0, "inches")
    else
        unit(bounds[4L], "inches")
}

rasterGrob <- function(image,
                       x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                       width=NULL, height=NULL,
                       just="centre", hjust=NULL, vjust=NULL,
                       interpolate=TRUE,
                       default.units="npc",
                       name=NULL, gp=gpar(), vp=NULL) {

    if (inherits(image, "nativeRaster"))
        raster <- image
    else
        raster <- as.raster(image)
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.null(width) && !is.unit(width))
        width <- unit(width, default.units)
    if (!is.null(height) && !is.unit(height))
        height <- unit(height, default.units)
    grob(raster=raster, x=x, y=y, width=width, height=height, just=just,
         hjust=hjust, vjust=vjust, interpolate=interpolate,
         name=name, gp=gp, vp=vp, cl="rastergrob")
}

grid.raster <- function(image,
                        x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                        width=NULL, height=NULL,
                        just="centre", hjust=NULL, vjust=NULL,
                        interpolate=TRUE,
                        default.units="npc",
                        name=NULL, gp=gpar(), vp=NULL) {
    rg <- rasterGrob(image,
                     x=x, y=y, width=width, height=height, just=just,
                     hjust=hjust, vjust=vjust, interpolate=interpolate,
                     default.units=default.units,
                     name=name, gp=gp, vp=vp)
    grid.draw(rg)
}

######################################
# TEXT primitive
######################################
validDetails.text <- function(x) {
  if (!is.language(x$label))
    x$label <- as.character(x$label)
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  x$rot <- as.numeric(x$rot)
  if (!all(is.finite(x$rot)) || length(x$rot) == 0)
    stop("invalid 'rot' value")
  valid.just(x$just)
  if (!is.null(x$hjust))
    x$hjust <- as.numeric(x$hjust)
  if (!is.null(x$vjust))
    x$vjust <- as.numeric(x$vjust)
  x$check.overlap <- as.logical(x$check.overlap)
  x
}

drawDetails.text <- function(x, recording=TRUE) {
  grid.Call.graphics(C_text, as.graphicsAnnot(x$label),
                     x$x, x$y,
                     resolveHJust(x$just, x$hjust),
                     resolveVJust(x$just, x$vjust),
                     x$rot, x$check.overlap)
}

xDetails.text <- function(x, theta) {
  bounds <- grid.Call(C_textBounds, as.graphicsAnnot(x$label),
                      x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot, theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[1L], "inches")
}

yDetails.text <- function(x, theta) {
  bounds <- grid.Call(C_textBounds, as.graphicsAnnot(x$label),
                      x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot, theta)
  if (is.null(bounds))
    unit(0.5, "npc")
  else
    unit(bounds[2L], "inches")
}

widthDetails.text <- function(x) {
  bounds <- grid.Call(C_textBounds, as.graphicsAnnot(x$label),
                      x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.text <- function(x) {
  bounds <- grid.Call(C_textBounds, as.graphicsAnnot(x$label),
                      x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

ascentDetails.text <- function(x) {
    if (length(x$label) == 1) {
        metrics <- grid.Call(C_stringMetric, as.graphicsAnnot(x$label))
        unit(metrics[[1]], "inches")
    } else {
        heightDetails(x)
    }
}

descentDetails.text <- function(x) {
    if (length(x$label) == 1) {
        metrics <- grid.Call(C_stringMetric, as.graphicsAnnot(x$label))
        unit(metrics[[2]], "inches")
    } else {
        unit(0, "inches")
    }
}

textGrob <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     rot=0, check.overlap=FALSE,
                     default.units="npc",
                     name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(label=label, x=x, y=y, just=just, hjust=hjust, vjust=vjust,
       rot=rot, check.overlap=check.overlap,
       name=name, gp=gp, vp=vp, cl="text")
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      just="centre", hjust=NULL, vjust=NULL,
                      rot=0, check.overlap=FALSE,
                      default.units="npc",
                      name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  tg <- textGrob(label=label, x=x, y=y, just=just,
                 hjust=hjust, vjust=vjust, rot=rot,
                 check.overlap=check.overlap,
                 default.units=default.units,
                 name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(tg)
  invisible(tg)
}

######################################
# POINTS primitive
######################################
valid.pch <- function(pch) {
  if (length(pch) == 0L)
    stop("zero-length 'pch'")
  if (is.null(pch))
    pch <- 1L
  else if (!is.character(pch))
    pch <- as.integer(pch)
  pch
}

validDetails.points <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$size))
    stop("'x', 'y' and 'size' must be units")
  if (length(x$x) != length(x$y))
    stop("'x' and 'y' must be 'unit' objects and have the same length")
  x$pch <- valid.pch(x$pch)
  x
}

drawDetails.points <- function(x, recording=TRUE) {
  grid.Call.graphics(C_points, x$x, x$y, x$pch, x$size)
}

# FIXME:  does not take into account the size of the symbols
xDetails.points <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.points <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

widthDetails.points <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[3L], "inches")
}

heightDetails.points <- function(x) {
  bounds <- grid.Call(C_locnBounds, x$x, x$y, 0)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4L], "inches")
}

pointsGrob <- function(x=stats::runif(10),
                       y=stats::runif(10),
                       pch=1, size=unit(1, "char"),
                       default.units="native",
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, pch=pch, size=size,
       name=name, gp=gp, vp=vp, cl="points")
}

grid.points <- function(x=stats::runif(10),
                        y=stats::runif(10),
                        pch=1, size=unit(1, "char"),
                        default.units="native",
                        name=NULL, gp=gpar(),
                        draw=TRUE, vp=NULL) {
  pg <- pointsGrob(x=x, y=y, pch=pch, size=size,
                   default.units=default.units,
                   name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(pg)
  invisible(pg)
}

######################################
# CLIP primitive
######################################
validDetails.clip <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$width) ||
      !is.unit(x$height))
    stop("'x', 'y', 'width', and 'height' must be units")
  if (length(x$x) > 1 || length(x$y) > 1 ||
      length(x$width) > 1 || length(x$height) > 1)
    stop("'x', 'y', 'width', and 'height' must all be units of length 1")
  valid.just(x$just)
  if (!is.null(x$hjust))
    x$hjust <- as.numeric(x$hjust)
  if (!is.null(x$vjust))
    x$vjust <- as.numeric(x$vjust)
  x
}

drawDetails.clip <- function(x, recording=TRUE) {
  grid.Call.graphics(C_clip, x$x, x$y, x$width, x$height,
                     resolveHJust(x$just, x$hjust),
                     resolveVJust(x$just, x$vjust))
}

clipGrob <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     width=unit(1, "npc"), height=unit(1, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     default.units="npc",
                     name=NULL, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  grob(x=x, y=y, width=width, height=height, just=just,
       hjust=hjust, vjust=vjust,
       name=name, vp=vp, cl="clip")
}

grid.clip <- function(...) {
  grid.draw(clipGrob(...))
}


######################################
# NULL primitive
######################################

validDetails.null <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  if (length(x$x) > 1 || length(x$y) > 1)
    stop("'x' and 'y' must all be units of length 1")
  x
}

drawDetails.null <- function(x, recording=TRUE) {
    # Deliberate null op.
    # NOTE: nothing will go on the graphics engine DL
    # This is ok I think because these grobs are only
    # useful on the grid DL (for other grid code to query
    # their size or location).
}

xDetails.null <- function(x, theta) {
    bounds <- grid.Call(C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[1L], "inches")
}

yDetails.null <- function(x, theta) {
    bounds <- grid.Call( C_locnBounds, x$x, x$y, theta)
    if (is.null(bounds))
        unit(0.5, "npc")
    else
        unit(bounds[2L], "inches")
}

# Deliberately ZERO
widthDetails.null <- function(x) {
    unit(0, "inches")
}

heightDetails.null <- function(x) {
    unit(0, "inches")
}

# A grob with GUARANTEED zero-width
# also GUARANTEED NOT to draw anything
nullGrob <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     default.units="npc",
                     name=NULL, vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    grob(x=x, y=y, name=name, vp=vp, cl="null")
}

# Convenient way to get nullGrob on the grid display list
grid.null <- function(...) {
    grid.draw(nullGrob(...))
}










