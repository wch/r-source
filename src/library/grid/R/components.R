######################################
# Default COLLECTION of grobs
######################################
draw.details.collection <- function(x, x.wrapped, recording=TRUE) {
  # A collection draws all of its children
  lapply(x$children, grid.draw, recording=FALSE)
}

# Have a draw=T argument because "only" other alternative is to
# have a separate make.collection function with identical argument
# list (i.e., duplicated entry point).  Not such an issue here,
# but just gets worse the more complex the graphical object gets.
grid.collection <- function(..., gp=gpar(), draw=TRUE, vp=NULL) {
  children <- list(...)
  # Allow for single argument of a list of grobs (rather than
  # multiple grobs as separate arguments)
  if (!is.grob(children[[1]]) && is.list(children[[1]]))
    children <- children[[1]]
  collection <- list(children=children, gp=gp, vp=vp)
  cl <- "collection"
  grid.grob(collection, cl, draw)
}

######################################
# AXES
######################################

# Axes are extended from the "collection" class
# They have named children and the same grobs stored in their
# children slot.  This means that the standard (e.g., draw.details)
# methods for collections will apply, but axes can allow more
# convenient access and more sophisticated control of their
# child grobs AS WELL

# The children of an axis are fixed to be:
# [[1]] major line
# [[2]] tick marks
# [[3]] tick labels

# NOTE that the `at' parameter is numeric (i.e., NOT a unit) for
# grid.xaxis and grid.yaxis.  These functions assume a unit for the `at'
# values rather than letting the user specify a unit.

draw.details.xaxis <- function(x, x.wrapped, recording=TRUE) {
  # We may have to create the children if there was not
  # enough information available at creation time
  if (is.null(x$at)) {
    # FIXME:  There should be a grid.pretty rather than
    # forcing users to use grid.Call
    at <- grid.pretty(current.viewport()$xscale)
    # We edit the grob itself so that the change is permanent
    grid.edit(x.wrapped, at=at, redraw=FALSE)
    # Then we make sure the current draw is aware of the change
    x <- grid.get(x.wrapped)
  }
  NextMethod()
}

# NOTE that this can't be for all axes because it needs to
# call make.XAXIS.ticks and make.XAXIS.labels
editDetails.xaxis <- function(x, new.values) {
  slot.names <- names(new.values)
  if (match("at", slot.names, nomatch=0)) {
    # NOTE that grid.edit has already set x$at to the new value
    # We might set at to NULL to get ticks recalculated at redraw
    if (!is.null(x$at)) {
      x$major <- make.xaxis.major(x$at, x$main)
      x$ticks <- make.xaxis.ticks(x$at, x$main)
      if (x$label)
        x$labels <- make.xaxis.labels(x$at, x$main)
      else
        x$labels <- NULL
      x$children <- list(x$major, x$ticks, x$labels)
    }
  }
  # FIXME:  Handle "label=" and "main=" too ?
  x
}

make.xaxis.major <- function(at, main) {
  if (main)
    y <- c(0, 0)
  else
    y <- c(1, 1)
  grid.lines(unit(c(min(at), max(at)), "native"),
         unit(y, "npc"), draw=FALSE)
}

make.xaxis.ticks <- function(at, main) {
  if (main) {
    tick.y0 <- unit(0, "npc")
    tick.y1 <- unit(-.5, "lines")
  }
  else {
    tick.y0 <- unit(1, "npc")
    tick.y1 <- unit(1, "npc") + unit(.5, "lines")
  }
  grid.segments(unit(at, "native"), tick.y0,
                unit(at, "native"), tick.y1,
                draw=FALSE)
}

make.xaxis.labels <- function(at, main) {
  # FIXME:  labels only character versions of "at"
  if (main)
    label.y <- unit(-1.5, "lines")
  else
    label.y <- unit(1, "npc") + unit(1.5, "lines")
  grid.text(as.character(at), unit(at, "native"), label.y,
                    just="centre", rot=0,
                    check.overlap=TRUE, draw=FALSE)
}

# The "main" x-axis is on the bottom when vp$origin is "bottom.*"
# and on the top when vp$origin is "top.*"
grid.xaxis <- function(at=NULL, label = TRUE, main=TRUE, gp=gpar(),
                   draw=TRUE, vp=NULL) {
  if (is.null(at))
    if (is.null(vp)) {
      # We do not have enough information to make the ticks and labels
      major <- NULL
      ticks <- NULL
      labels <- NULL
    }
    else
      at <- grid.pretty(vp$xscale)
  if (!is.null(at)) {
    major <- make.xaxis.major(at, main)
    ticks <- make.xaxis.ticks(at, main)
    if (label)
      labels <- make.xaxis.labels(at, main)
    else
      labels <- NULL
  }
  grid.grob(list(at=at, children=list(major, ticks, labels),
                 major=major, ticks=ticks, labels=labels,
                 label=label, gp=gp, main=main, vp=vp),
            c("xaxis", "axis", "collection"), draw)
}

draw.details.yaxis <- function(x, x.wrapped, recording=TRUE) {
  # We may have to create the children if there was not
  # enough information available at creation time
  if (is.null(x$at)) {
    at <- grid.pretty(current.viewport()$yscale)
    grid.edit(x.wrapped, at=at, redraw=FALSE)
    x <- grid.get(x.wrapped)
  }
  NextMethod()
}

editDetails.yaxis <- function(x, new.values) {
  slot.names <- names(new.values)
  if (match("at", slot.names, nomatch=0)) {
    if (!is.null(x$at)) {
      x$major <- make.yaxis.major(x$at, x$main)
      x$ticks <- make.yaxis.ticks(x$at, x$main)
      if (x$label)
        x$labels <- make.yaxis.labels(x$at, x$main)
      else
        x$labels <- NULL
      x$children <- list(x$major, x$ticks, x$labels)
    }
  }
  x
}

make.yaxis.major <- function(at, main) {
  if (main)
    x <- c(0, 0)
  else
    x <- c(1, 1)
  grid.lines(unit(x, "npc"), unit(c(min(at), max(at)), "native"), draw=FALSE)
}

make.yaxis.ticks <- function(at, main) {
  if (main) {
    tick.x0 <- unit(0, "npc")
    tick.x1 <- unit(-.5, "lines")
  }
  else {
    tick.x0 <- unit(1, "npc")
    tick.x1 <- unit(1, "npc") + unit(.5, "lines")
  }
  grid.segments(tick.x0, unit(at, "native"),
                tick.x1, unit(at, "native"),
                draw=FALSE)
}

make.yaxis.labels <- function(at, main) {
  if (main) {
    hjust <- "right"
    label.x <- unit(-1, "lines")
  }
  else {
    hjust <- "left"
    label.x <- unit(1, "npc") + unit(1, "lines")
  }
  just <- c(hjust, "centre")
  grid.text(as.character(at), label.x, unit(at, "native"),
        just=just, rot=0, check.overlap=TRUE, draw=FALSE)
}

# The "main" y-axis is on the left when vp$origin is "*.left"
# and on the right when vp$origin is "*.right"
grid.yaxis <- function(at=NULL, label=TRUE, main=TRUE, gp=gpar(),
                   draw=TRUE, vp=NULL) {
  if (is.null(at))
    if (is.null(vp)) {
      # We do not have enough information to make the ticks and labels
      major <- NULL
      ticks <- NULL
      labels <- NULL
    }
    else
      at <- grid.pretty(vp$yscale)
  if (!is.null(at)) {
    major <- make.yaxis.major(at, main)
    ticks <- make.yaxis.ticks(at, main)
    if (label)
      labels <- make.yaxis.labels(at, main)
    else
      labels <- NULL
  }
  grid.grob(list(at=at, major=major, ticks=ticks, labels=labels,
                 children=list(major, ticks, labels),
                 label=label, gp=gp, main=main, vp=vp),
            c("yaxis", "axis", "collection"), draw)
}

######################################
# Simple "side-effect" plotting functions
######################################

grid.grill <- function(h=unit(seq(0.25, 0.75, 0.25), "npc"),
                       v=unit(seq(0.25, 0.75, 0.25), "npc"),
                       default.units="npc",
                       gp=gpar(col="grey"), vp=NULL) {
  if (!is.unit(h))
    h <- unit(h, default.units)
  if (!is.unit(v))
    v <- unit(v, default.units)
  # FIXME:  Should replace for loop and call to grid.lines with call to grid.segments
  # once the latter exists
  if (!is.null(vp))
    push.viewport(vp)
  grid.segments(v, unit(0, "npc"), v, unit(1, "npc"), gp=gp)
  grid.segments(unit(0, "npc"), h, unit(1, "npc"), h, gp=gp)
  if (!is.null(vp))
    pop.viewport()
}

