#  File src/library/grid/R/viewport.R
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


initvpAutoName <- function() {
  index <- 0
  function() {
    index <<- index + 1
    paste0("GRID.VP.", index)
  }
}

vpAutoName <- initvpAutoName()

# NOTE: The order of the elements in viewports and pushedvps are
# VERY IMPORTANT because the C code accesses them using constant
# indices (i.e., if you change the order here the world will end!
valid.viewport <- function(x, y, width, height, just,
                           gp, clip,
                           xscale, yscale, angle,
                           layout, layout.pos.row, layout.pos.col,
                           name) {
  if (length(x) > 1 || length(y) > 1 ||
      length(width) > 1 || length(height) > 1)
    stop("'x', 'y', 'width', and 'height' must all be units of length 1")
  if (!is.gpar(gp))
    stop("invalid 'gp' value")
  if (!is.logical(clip))
    clip <- switch(as.character(clip),
                   on=TRUE,
                   off=NA,
                   inherit=FALSE,
                   stop("invalid 'clip' value"))
  # Ensure both 'xscale' and 'yscale' are numeric (brute force defense)
  xscale <- as.numeric(xscale)
  yscale <- as.numeric(yscale)
  if (!is.numeric(xscale) || length(xscale) != 2 ||
      any(!is.finite(xscale)) || diff(xscale) == 0)
    stop("invalid 'xscale' in viewport")
  if (!is.numeric(yscale) || length(yscale) != 2 ||
      any(!is.finite(yscale)) || diff(yscale) == 0)
    stop("invalid 'yscale' in viewport")
  if (!is.numeric(angle) || length(angle) != 1 ||
      !is.finite(angle))
    stop("invalid 'angle' in viewport")
  if (!(is.null(layout) || is.layout(layout)))
    stop("invalid 'layout' in viewport")
  if (!is.null(layout.pos.row)) {
    layout.pos.row <- as.integer(range(layout.pos.row))
    if (any(!is.finite(layout.pos.row)))
      stop("invalid 'layout.pos.row' in viewport")
  }
  if (!is.null(layout.pos.col)) {
    layout.pos.col <- as.integer(range(layout.pos.col))
    if (any(!is.finite(layout.pos.col)))
      stop("invalid 'layout.pos.col' in viewport")
  }
  # If name is NULL then we give it a default
  # Otherwise it should be a valid R name
  if (is.null(name))
    name <- vpAutoName()
  # Put all the valid things first so that are found quicker
  vp <- list(x = x, y = y, width = width, height = height,
             justification = just,
             gp = gp,
             clip = clip,
             xscale = xscale,
             yscale = yscale,
             angle = angle,
             layout = layout,
             layout.pos.row = layout.pos.row,
             layout.pos.col = layout.pos.col,
             valid.just = valid.just(just),
             valid.pos.row = layout.pos.row,
             valid.pos.col = layout.pos.col,
             name=name)
  class(vp) <- "viewport"
  vp
}

# When a viewport is pushed, an internal copy is stored along
# with plenty of additional information relevant to the state
# at the time of being pushed (this is all used to return to this
# viewport without having to repush it)
pushedvp <- function(vp) {
    # NOTE that this function is only called from C code:
    # either directly from L_setviewport() or indirectly from initVP()
    # via grid.top.level.vp()
    # vp$gpar and vp$parentgpar are both set previously in push.vp.viewport()
  pvp <- c(vp, list(trans = NULL,
                    widths = NULL,
                    heights = NULL,
                    width.cm = NULL,
                    height.cm = NULL,
                    rotation = NULL,
                    cliprect = NULL,
                    parent = NULL,
                    # Children of this pushedvp will be stored
                    # in an environment
                    children = new.env(hash=TRUE, parent=baseenv()),
                    # Initial value of 0 means that the viewport will
                    # be pushed "properly" the first time, calculating
                    # transformations, etc ...
                    devwidthcm = 0,
                    devheightcm = 0))
  class(pvp) <- c("pushedvp", class(vp))
  pvp
}

vpFromPushedvp <- function(pvp) {
  vp <- pvp[c("x", "y", "width", "height",
              "justification", "gp", "clip",
              "xscale", "yscale", "angle",
              "layout", "layout.pos.row", "layout.pos.col",
              "valid.just", "valid.pos.row", "valid.pos.col",
              "name")]
  class(vp) <- "viewport"
  vp
}

as.character.viewport <- function(x, ...) {
  paste0("viewport[", x$name, "]")
}

as.character.vpList <- function(x, ...) {
  paste0("(", paste(vapply(x, as.character, ""), collapse=", "), ")")
}

as.character.vpStack <- function(x, ...) {
  paste(vapply(x, as.character, ""), collapse="->")
}

as.character.vpTree <- function(x, ...) {
  paste(x$parent, x$children, sep="->")
}

print.viewport <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}

width.details.viewport <- function(x) {
  absolute.size(x$width)
}

height.details.viewport <- function(x) {
  absolute.size(x$height)
}

# How many "levels" in viewport object
depth <- function(x, ...) {
  UseMethod("depth")
}

depth.viewport <- function(x, ...) {
  1
}

depth.vpList <- function(x, ...) {
  # When pushed, the last element of the vpList is pushed last
  # so we are left whereever that leaves us
  depth(x[[length(x)]], ...)
}

depth.vpStack <- function(x, ...) {
  # Elements in the stack may be vpStacks or vpLists or vpTrees
  # so need to sum all the depths
  sum(sapply(x, depth, ..., simplify=TRUE))
}

depth.vpTree <- function(x, ...) {
  # When pushed, the last element of the vpTree$children is
  # pushed last so we are left wherever that leaves us
  depth(x$parent, ...) + depth(x$children[[length(x$children)]], ...)
}

depth.path <- function(x, ...) {
  x$n
}

####################
# Accessors
####################

viewport.layout <- function(vp) {
  vp$layout
}

viewport.transform <- function(vp) {
    .Defunct("current.transform")
}

####################
# Public Constructor
####################
viewport <- function(x = unit(0.5, "npc"),
                     y = unit(0.5, "npc"),
                     width = unit(1, "npc"),
                     height = unit(1, "npc"),
                     default.units = "npc",
                     just = "centre",
                     gp = gpar(),
                     clip = "inherit",
                     # FIXME: scales are only linear at the moment
                     xscale = c(0, 1),
                     yscale = c(0, 1),
                     angle = 0,
                     # Layout for arranging children of this viewport
                     layout = NULL,
                     # Position of this viewport in parent's layout
                     layout.pos.row = NULL,
                     layout.pos.col = NULL,
                     # This is down here to avoid breaking
                     # existing code
                     name=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  valid.viewport(x, y, width, height, just,
                 gp, clip, xscale, yscale, angle,
                 layout, layout.pos.row, layout.pos.col, name)
}

is.viewport <- function(vp) {
  inherits(vp, "viewport")
}

#############
# Some classes derived from viewport
#############

viewportorpath <- function(x) {
    is.viewport(x) || inherits(x, "vpPath")
}

vpListFromList <- function(vps) {
  if (all(sapply(vps, viewportorpath, simplify=TRUE))) {
    class(vps) <- c("vpList", "viewport")
    vps
  } else {
    stop("only viewports allowed in 'vpList'")
  }
}

# Viewports will be pushed in parallel
vpList <- function(...) {
  vps <- list(...)
  vpListFromList(vps)
}

# Viewports will be pushed in series
vpStack <- function(...) {
  vps <- list(...)
  if (all(sapply(vps, viewportorpath, simplify=TRUE))) {
    class(vps) <- c("vpStack", "viewport")
    vps
  } else {
    stop("only viewports allowed in 'vpStack'")
  }
}

# Viewports will be pushed as a tree
vpTree <- function(parent, children) {
  if (viewportorpath(parent) && inherits(children, "vpList")) {
    tree <- list(parent=parent, children=children)
    class(tree) <- c("vpTree", "viewport")
    tree
  } else {
    stop("'parent' must be a viewport and 'children' must be a 'vpList' in 'vpTree'")
  }
}

# A function for setting all gpars for vpStack/List/Tree
# Used in size.R
setvpgpar <- function(vp) {
  UseMethod("setvpgpar")
}

setvpgpar.viewport <- function(vp) {
  if (!is.null(vp$gp))
    set.gpar(vp$gp)
}

setvpgpar.vpStack <- function(vp) {
  lapply(vp, setvpgpar)
}

setvpgpar.vpList <- function(vp) {
  setvpgpar(vp[[length(vp)]])
}

setvpgpar.vpTree <- function(vp) {
  setvpgpar(vp$parent)
  setvpgpar(vp$children)
}

#############
# Functions for creating "paths" of viewport names
#############
.grid.pathSep <- "::"

vpPathFromVector <- function(names) {
  if (any(bad <- !is.character(names)))
      stop(ngettext(sum(bad),
                    "invalid viewport name",
                    "invalid viewport names"),
           domain = NA)
  names <- unlist(strsplit(names, .grid.pathSep))
  n <- length(names)
  if (n < 1)
    stop("a viewport path must contain at least one viewport name")
  path <- list(path=if (n==1) NULL else
               paste(names[seq_len(n-1L)], collapse=.grid.pathSep),
               name=names[n],
               n=n)
  class(path) <- c("vpPath", "path")
  path
}

vpPath <- function(...) {
  names <- c(...)
  vpPathFromVector(names)
}

as.character.path <- function(x, ...) {
  if (x$n == 1)
    x$name
  else
    paste(x$path, x$name, sep=.grid.pathSep)
}

print.path <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}

`[.vpPath` <- function(x, index, ...) {
  names <- unlist(strsplit(as.character(x), .grid.pathSep))[index]
  vpPathFromVector(names)
}

# Explode path$path
explode <- function(x) {
    UseMethod("explode")
}

explode.character <- function(x) {
    unlist(strsplit(x, .grid.pathSep))
}

explode.path <- function(x) {
  if (x$n == 1)
    x$name
  else
    c(explode(x$path), x$name)
}


#############
# Some handy viewport functions
#############

# Create a viewport with margins given in number of lines
plotViewport <- function(margins=c(5.1, 4.1, 4.1, 2.1), ...) {
  margins <- rep(as.numeric(margins), length.out=4)
  viewport(x=unit(margins[2L], "lines"),
           width=unit(1, "npc") - unit(sum(margins[c(2,4)]), "lines"),
           y=unit(margins[1L], "lines"),
           height=unit(1, "npc") - unit(sum(margins[c(1,3)]), "lines"),
           just=c("left", "bottom"),
           ...)
}

# Create a viewport from data
# If xscale not specified then determine from x
# If yscale not specified then determine from y
dataViewport <- function(xData = NULL, yData = NULL,
                         xscale = NULL, yscale = NULL, extension = 0.05, ...)
{
    extension <- rep(extension, length.out = 2)
    if (is.null(xscale)) {
        if (is.null(xData))
            stop("must specify at least one of 'x' or 'xscale'")
        xscale <- extendrange(xData, f = extension[1L])
    }
    if (is.null(yscale)) {
        if (is.null(yData))
            stop("must specify at least one of 'y' or 'yscale'")
        yscale <- extendrange(yData, f = extension[2L])
    }
    viewport(xscale = xscale, yscale = yscale, ...)
}
