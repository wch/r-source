
initvpAutoName <- function() {
  index <- 0
  function() {
    index <<- index + 1
    paste("GRID.VP.", index, sep="")
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
  if (unit.length(x) > 1 || unit.length(y) > 1 ||
      unit.length(width) > 1 || unit.length(height) > 1)
    stop("`x', `y', `width', and `height' must all be units of length 1")
  if (!is.gpar(gp))
    stop("Invalid graphics parameters")
  if (!is.logical(clip)) 
    clip <- switch(as.character(clip),
                   on=TRUE,
                   off=NA,
                   inherit=FALSE,
                   stop("Invalid clip value"))
  if (!is.numeric(xscale) || length(xscale) != 2 ||
      any(!is.finite(xscale)))
    stop("Invalid xscale in viewport")
  if (!is.numeric(yscale) || length(yscale) != 2 ||
      any(!is.finite(yscale)))
    stop("Invalid yscale in viewport")
  if (!is.numeric(angle) || length(angle) != 1)
    stop("Invalid angle in viewport")
  if (!(is.null(layout) || is.layout(layout)))
    stop("Invalid layout in viewport")
  if (!is.null(layout.pos.row))
    layout.pos.row <- as.integer(rep(range(layout.pos.row), length.out=2))
  if (!is.null(layout.pos.col))
    layout.pos.col <- as.integer(rep(range(layout.pos.col), length.out=2))
  # If name is NULL then we give it a default
  # Otherwise it should be a valid R name
  if (is.null(name))
    name <- vpAutoName()
  else
    name <- make.names(name)
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
  pvp <- c(vp, list(gpar = NULL,
                    trans = NULL,
                    widths = NULL,
                    heights = NULL,
                    width.cm = NULL,
                    height.cm = NULL,
                    rotation = NULL,
                    cliprect = NULL,
                    parent = NULL,
                    # Children of this pushedvp will be stored
                    # in an environment
                    children = new.env(hash=TRUE, parent=NULL),
                    # Initial value of 0 means that the viewport will
                    # be pushed "properly" the first time, calculating
                    # transformations, etc ...
                    devwidthcm = 0,
                    devheightcm = 0,
                    # This is down here because need to keep
                    # #defines in grid.h consistent with order here
                    parentgpar = NULL))
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
  paste("viewport[", x$name, "]", sep="")
}

as.character.vpList <- function(x, ...) {
  paste("(", paste(sapply(x, as.character, simplify=TRUE), collapse=", "),
        ")", sep="")
}

as.character.vpStack <- function(x, ...) {
  paste(sapply(x, as.character, simplify=TRUE), collapse="->")
}

as.character.vpTree <- function(x, ...) {
  paste(x$parent, x$children, sep="->")
}

print.viewport <- function(x, ...) {
  cat(as.character(x), "\n")
}

width.details.viewport <- function(x) {
  absolute.size(x$width)
}

height.details.viewport <- function(x) {
  absolute.size(x$height)
}

# How many "levels" in viewport object
depth <- function(vp) {
  UseMethod("depth")
}

depth.viewport <- function(vp) {
  1
}

depth.vpList <- function(vp) {
  # When pushed, the last element of the vpList is pushed last
  # so we are left whereever that leaves us
  depth(vp[[length(vp)]])
}

depth.vpStack <- function(vp) {
  # Elements in the stack may be vpStacks or vpLists or vpTrees
  # so need to sum all the depths
  sum(sapply(vp, depth, simplify=TRUE))
}

depth.vpTree <- function(vp) {
  # When pushed, the last element of the vpTree$children is
  # pushed last so we are left wherever that leaves us
  depth(vp$parent) + depth(vp$children[[length(vp$children)]])
}

depth.path <- function(path) {
  path$n
}

####################
# Accessors
####################

viewport.layout <- function(vp) {
  vp$layout
}

viewport.transform <- function(vp) {
  .Deprecated("current.transform")
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

vpListFromList <- function(vps) {
  if (all(sapply(vps, is.viewport, simplify=TRUE))) {
    class(vps) <- c("vpList", "viewport")
    vps
  } else {
    stop("Only viewports allowed in vpList")
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
  if (all(sapply(vps, is.viewport, simplify=TRUE))) {
    class(vps) <- c("vpStack", "viewport")
    vps
  } else {
    stop("Only viewports allowed in vpStack")
  }
}

# Viewports will be pushed as a tree
vpTree <- function(parent, children) {
  if (is.viewport(parent) && inherits(children, "vpList")) {
    tree <- list(parent=parent, children=children)
    class(tree) <- c("vpTree", "viewport")
    tree
  } else {
    stop("Parent must be viewport and children must be vpList in vpTree")
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

# Functions for creating "paths" of viewport names
.grid.pathSep <- "::"

vpPathFromVector <- function(names) {
  n <- length(names)
  if (n < 1)
    stop("A viewport path must contain at least one viewport name")
  if (!all(is.character(names)))
    stop("Invalid viewport name(s)")
  path <- list(path=if (n==1) NULL else
               paste(names[1:(n-1)], collapse=.grid.pathSep),
               name=names[n],
               n=n)
  class(path) <- c("vpPath", "path")
  path
}

vpPath <- function(...) {
  names <- c(...)
  vpPathFromVector(names)
}

# Create vpPath from string with embedded VpPathSep(s)
vpPathDirect <- function(path) {
  names <- unlist(strsplit(path, .grid.pathSep))
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
}

"[.vpPath" <- function(x, index, ...) {
  names <- unlist(strsplit(as.character(x), .grid.pathSep))[index]
  vpPathFromVector(names)
}

# Explode path$path
explodePath <- function(path) {
  unlist(strsplit(path, .grid.pathSep))
}

#############
# Some handy viewport functions
#############

# Create a viewport with margins given in number of lines
plotViewport <- function(margins, ...) {
  margins <- rep(as.numeric(margins), length.out=4)
  viewport(x=unit(margins[2], "lines"),
           width=unit(1, "npc") - unit(sum(margins[c(2,4)]), "lines"),
           y=unit(margins[1], "lines"),
           height=unit(1, "npc") - unit(sum(margins[c(1,3)]), "lines"),
           just=c("left", "bottom"),
           ...)
}

# Create a viewport from data
# If xscale not specified then determine from x
# If yscale not specified then determine from y
dataViewport <- function(xData=NULL, yData=NULL, xscale=NULL, yscale=NULL,
                         extension=0.05, ...) {
  if (is.null(xscale)) {
    if (is.null(xData))
      stop("Must specify at least one of x or xscale")
    xscale <- range(xData) + c(-1, 1)*diff(range(xData))*extension
  }
  if (is.null(yscale)) {
    if (is.null(yData))
      stop("Must specify at least one of y or yscale")
    yscale <- range(yData) + c(-1, 1)*diff(range(yData))*extension
  }
  viewport(xscale=xscale, yscale=yscale, ...)
}
