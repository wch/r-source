grid.collection <- function(..., gp=gpar(), draw=TRUE, vp=NULL) {
  .Deprecated("gTree")
  gc <- gTree(children=gList(...), gp=gp, vp=vp, cl="collection")
  if (draw)
    grid.draw(gc)
  gc
}

######################################
# AXES
######################################

# Axes are extended from the "gTree" class
# This means that the standard (e.g., draw.details)
# methods for gTrees will apply

# The children of an axis are fixed to be:

# NOTE that the `at' parameter is numeric (i.e., NOT a unit) for
# grid.xaxis and grid.yaxis.  These functions assume a unit for the `at'
# values rather than letting the user specify a unit.

validDetails.axis <- function(x) {
  if (!is.null(x$at))
    x$at <- as.numeric(x$at)
  x$label <- as.logical(x$label)
  x$main <- as.logical(x$main)
  x
}

drawDetails.xaxis <- function(x, recording=TRUE) {
  # If x$at is NULL, then we must calculate the
  # tick marks on-the-fly
  if (is.null(x$at)) {
    x$at <- grid.pretty(current.viewport()$xscale)
    # Add the new output as children
    x <- addGrob(x, make.xaxis.major(x$at, x$main))
    x <- addGrob(x, make.xaxis.ticks(x$at, x$main))
    if (x$label)
      x <- addGrob(x, make.xaxis.labels(x$at, x$main))
    else
      x <- removeGrob(x, "labels", warn=FALSE)
    # Apply any edits relevant to children
    x <- applyEdits(x, x$edits)
    # Draw the children
    for (i in childNames(x))
      grid.draw(getGrob(x, i))
  }
}

# NOTE that this can't be for all axes because it needs to
# call make.XAXIS.ticks and make.XAXIS.labels
editDetails.xaxis <- function(x, specs) {
  slot.names <- names(specs)
  if ("at" %in% slot.names) {
    # NOTE that grid.edit has already set x$at to the new value
    # We might set at to NULL to get ticks recalculated at redraw
    if (is.null(x$at)) {
      x <- removeGrob(x, "major", warn=FALSE)
      x <- removeGrob(x, "ticks", warn=FALSE)
      x <- removeGrob(x, "labels", warn=FALSE)
    } else {
      x <- addGrob(x, make.xaxis.major(x$at, x$main))
      x <- addGrob(x, make.xaxis.ticks(x$at, x$main))
      if (x$label)
        x <- addGrob(x, make.xaxis.labels(x$at, x$main))
      else
        x <- removeGrob(x, "labels", warn=FALSE)
    }
  }
  if ("label" %in% slot.names) {
    if (x$label)
      x <- addGrob(x, make.xaxis.labels(x$at, x$main))
    else
      x <- removeGrob(x, "labels", warn=FALSE)    
  }
  if ("main" %in% slot.names)
    if (!is.null(at)) {
      x <- addGrob(x, make.xaxis.major(x$at, x$main))
      x <- addGrob(x, make.xaxis.ticks(x$at, x$main))
      if (x$label)
        x <- addGrob(x, make.xaxis.labels(x$at, x$main))
      else
        x <- removeGrob(x, "labels", warn=FALSE)
    }
  x
}

make.xaxis.major <- function(at, main) {
  if (main)
    y <- c(0, 0)
  else
    y <- c(1, 1)
  linesGrob(unit(c(min(at), max(at)), "native"),
            unit(y, "npc"), name="major")
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
  segmentsGrob(unit(at, "native"), tick.y0,
               unit(at, "native"), tick.y1,
               name="ticks")
}

make.xaxis.labels <- function(at, main) {
  # FIXME:  labels only character versions of "at"
  if (main)
    label.y <- unit(-1.5, "lines")
  else
    label.y <- unit(1, "npc") + unit(1.5, "lines")
  textGrob(as.character(at), unit(at, "native"), label.y,
           just="centre", rot=0,
           check.overlap=TRUE, name="labels")
}

xaxisGrob <- function(at=NULL, label=TRUE, main=TRUE,
                      edits=NULL, 
                      name=NULL, gp=gpar(), vp=NULL) {
  grid.xaxis(at=at, label=label, main=main,
             edits=edits,
             name=name, gp=gp, draw=FALSE, vp=vp)
}

# The "main" x-axis is on the bottom when vp$origin is "bottom.*"
# and on the top when vp$origin is "top.*"
grid.xaxis <- function(at=NULL, label=TRUE, main=TRUE,
                       edits=NULL, name=NULL, gp=gpar(),
                       draw=TRUE, vp=NULL) {
  if (is.null(at)) {
    # We do not have enough information to make the ticks and labels
    major <- NULL
    ticks <- NULL
    labels <- NULL
  } else {
    major <- make.xaxis.major(at, main)
    ticks <- make.xaxis.ticks(at, main)
    if (label)
      labels <- make.xaxis.labels(at, main)
    else
      labels <- NULL
  }
  xg <- applyEdits(gTree(at=at, label=label, main=main,
                         children=gList(major, ticks, labels),
                         edits=edits,
                         name=name, gp=gp, vp=vp,
                         cl=c("xaxis", "axis")),
                   edits)
  if (draw)
    grid.draw(xg)
  invisible(xg)
}

drawDetails.yaxis <- function(x, recording=TRUE) {
  # If x$at is NULL, then we must calculate and draw the
  # tick marks on-the-fly
  if (is.null(x$at)) {
    x$at <- grid.pretty(current.viewport()$yscale)
    x <- addGrob(x, make.yaxis.major(x$at, x$main))
    x <- addGrob(x, make.yaxis.ticks(x$at, x$main))
    if (x$label)
      x <- addGrob(x, make.yaxis.labels(x$at, x$main))
    else
      x <- removeGrob(x, "labels", warn=FALSE)
    x <- applyEdits(x, x$edits)
    for (i in childNames(x))
      grid.draw(getGrob(x, i))
  }
}

editDetails.yaxis <- function(x, specs) {
  slot.names <- names(specs)
  if ("at" %in% slot.names) {
    if (is.null(x$at)) {
      x <- removeGrob(x, "major", warn=FALSE)
      x <- removeGrob(x, "ticks", warn=FALSE)
      x <- removeGrob(x, "labels", warn=FALSE)
    } else {
      x <- addGrob(x, make.yaxis.major(x$at, x$main))
      x <- addGrob(x, make.yaxis.ticks(x$at, x$main))
      if (x$label)
        x <- addGrob(x, make.yaxis.labels(x$at, x$main))
      else
        x <- removeGrob(x, "labels", warn=FALSE)
    }
  }
  if ("label" %in% slot.names) {
    if (x$label)
      x <- addGrob(x, make.xaxis.labels(x$at, x$main))
    else
      x <- removeGrob(x, "labels", warn=FALSE)    
  }
  if ("main" %in% slot.names)
    if (!is.null(at)) {
      x <- addGrob(x, make.yaxis.major(x$at, x$main))
      x <- addGrob(x, make.yaxis.ticks(x$at, x$main))
      if (x$label)
        x <- addGrob(x, make.yaxis.labels(x$at, x$main))
      else
        x <- removeGrob(x, "labels", warn=FALSE)
    }
  x
}

make.yaxis.major <- function(at, main) {
  if (main)
    x <- c(0, 0)
  else
    x <- c(1, 1)
  linesGrob(unit(x, "npc"), unit(c(min(at), max(at)), "native"),
            name="major")
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
  segmentsGrob(tick.x0, unit(at, "native"),
               tick.x1, unit(at, "native"),
               name="ticks")
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
  textGrob(as.character(at), label.x, unit(at, "native"),
           just=just, rot=0, check.overlap=TRUE, name="labels")
}

yaxisGrob <- function(at=NULL, label=TRUE, main=TRUE,
                      edits=NULL,
                      name=NULL, gp=gpar(), vp=NULL) {
  grid.yaxis(at=at, label=label, main=main, edits=edits,
             name=name, gp=gp, draw=FALSE, vp=vp)
}

# The "main" y-axis is on the left when vp$origin is "*.left"
# and on the right when vp$origin is "*.right"
grid.yaxis <- function(at=NULL, label=TRUE, main=TRUE,
                       edits=NULL,
                       name=NULL, gp=gpar(),
                       draw=TRUE, vp=NULL) {
  if (is.null(at)) {
    # We do not have enough information to make the ticks and labels
    major <- NULL
    ticks <- NULL
    labels <- NULL
  } else {
    major <- make.yaxis.major(at, main)
    ticks <- make.yaxis.ticks(at, main)
    if (label)
      labels <- make.yaxis.labels(at, main)
    else
      labels <- NULL
  }
  yg <- applyEdits(gTree(at=at, label=label, main=main, 
                         children=gList(major, ticks, labels),
                         edits=edits,
                         name=name, gp=gp, vp=vp,
                         cl=c("yaxis", "axis")),
                   edits)
  if (draw)
    grid.draw(yg)
  invisible(yg)
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
    pushViewport(vp)
  grid.segments(v, unit(0, "npc"), v, unit(1, "npc"), gp=gp)
  grid.segments(unit(0, "npc"), h, unit(1, "npc"), h, gp=gp)
  if (!is.null(vp))
    popViewport()
}

