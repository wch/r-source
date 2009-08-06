#  File src/library/grid/R/viewport.R
#  Part of the R package, http://www.R-project.org
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
  if (length(x) > 1 || length(y) > 1 ||
      length(width) > 1 || length(height) > 1)
    stop("'x', 'y', 'width', and 'height' must all be units of length 1")
  if (!is.gpar(gp))
    stop("Invalid graphics parameters")
  if (!is.logical(clip))
    clip <- switch(as.character(clip),
                   on=TRUE,
                   off=NA,
                   inherit=FALSE,
                   stop("Invalid 'clip' value"))
  if (!is.numeric(xscale) || length(xscale) != 2 ||
      any(!is.finite(xscale)))
    stop("Invalid 'xscale' in viewport")
  if (!is.numeric(yscale) || length(yscale) != 2 ||
      any(!is.finite(yscale)))
    stop("Invalid 'yscale' in viewport")
  if (!is.numeric(angle) || length(angle) != 1 ||
      !is.finite(angle))
    stop("Invalid 'angle' in viewport")
  if (!(is.null(layout) || is.layout(layout)))
    stop("Invalid 'layout' in viewport")
  if (!is.null(layout.pos.row)) {
    layout.pos.row <- as.integer(range(layout.pos.row))
    if (any(!is.finite(layout.pos.row)))
      stop("Invalid 'layout.pos.row' in viewport")
  }
  if (!is.null(layout.pos.col)) {
    layout.pos.col <- as.integer(range(layout.pos.col))
    if (any(!is.finite(layout.pos.col)))
      stop("Invalid 'layout.pos.col' in viewport")
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
                    children = new.env(hash=TRUE, parent=baseenv()),
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
  invisible(x)
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
    stop("Only viewports allowed in 'vpList'")
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
    stop("Only viewports allowed in 'vpStack'")
  }
}

# Viewports will be pushed as a tree
vpTree <- function(parent, children) {
  if (is.viewport(parent) && inherits(children, "vpList")) {
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
  n <- length(names)
  if (n < 1)
    stop("A viewport path must contain at least one viewport name")
  if (!all(is.character(names)))
    stop("Invalid viewport name(s)")
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
  invisible(x)
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
# Viewing viewports
#############

# FIXME:  some of this code for vpLists and vpStacks and vpTrees
# assumes that the components of a vpList or vpStack or the
# vpTree parent can ONLY be a viewport (when in fact they can
# also be a vpList, vpStack, or vpTree!)

# Label a viewport
# Get physical aspect ratio of vp to determine whether to rotate
# Shrink text to fit in vp
# (Assumes that we are currently occupying 'vp'
#  so that conversions are correct)
labelVP <- function(vp, col) {
    vw <- convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
    vh <- convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE)
    tw <- convertWidth(stringWidth(vp$name), "inches", valueOnly=TRUE)
    th <- convertHeight(stringHeight(vp$name), "inches", valueOnly=TRUE)
    eps <- .01
    # If viewport is REALLY short, draw horiz at normal cex
    if (vh < eps) {
        rot <- 0
        cex <- 1
    # If viewport is REALLY thin, draw vertical at normal cex
    } else if (vw < eps) {
        rot <- 90
        cex <- 1
    } else {
        vratio <- vh/vw
        if (vratio > 1 && tw > vw) {
            rot <- 90
            wratio <- th/vw
            hratio <- tw/vh
        } else {
            rot <- 0
            wratio <- tw/vw
            hratio <- th/vh
        }
        if (wratio > 1 || hratio > 1) {
            cex <- 1/max(wratio, hratio)
        } else {
            cex <- 1
        }
    }
    # Violate any clipping that is in effect
    pushViewport(viewport(clip="off"))
    grid.text(vp$name, rot=rot, gp=gpar(col=col, cex=cex))
    upViewport()
}

# Draw a "viewport"
drawVP <- function(vp, curDepth, depth, col, fill, label) {
    UseMethod("drawVP")
}

drawVP.viewport <- function(vp, curDepth, depth, col, fill, label) {
    if (vp$name != "ROOT" &&
        (is.null(depth) || curDepth %in% depth)) {
        pushViewport(vp)
        colIndex <- (curDepth - 1) %% length(col) + 1
        fillIndex <- (curDepth - 1) %% length(fill) + 1
        grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
        if (label)
            labelVP(vp, col[colIndex])
        upViewport()
    }
}

drawVP.vpList <- function(vp, curDepth, depth, col, fill, label) {
    lapply(vp, drawVP, curDepth, depth, col, fill, label)
}

drawVPStack <- function(vp, curDepth, depth, col, fill, label) {
    pushViewport(vp)
    if (is.null(depth) || curDepth %in% depth) {
        colIndex <- (curDepth - 1) %% length(col) + 1
        fillIndex <- (curDepth - 1) %% length(fill) + 1
        grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
    }
}

drawVP.vpStack <- function(vp, curDepth, depth, col, fill, label) {
    d <- depth(vp)
    mapply(drawVPStack, vp, curDepth + 1:d - 1,
           MoreArgs=list(depth, col, fill, label))
    if (label && curDepth + d - 1 %in% depth)
        labelVP(vp[[d]], col[(curDepth + d - 2) %% length(col) + 1])
    upViewport(depth(vp))
}

drawVP.vpTree <- function(vp, curDepth, depth, col, fill, label) {
    if (vp$parent$name == "ROOT") {
        lapply(vp$children, drawVP, curDepth, depth, col, fill, label)
    } else {
        pushViewport(vp$parent)
        if (is.null(depth) || curDepth %in% depth) {
            colIndex <- (curDepth - 1) %% length(col) + 1
            fillIndex <- (curDepth - 1) %% length(fill) + 1
            grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
            if (label) {
                drawLabel <- is.null(vp$children) ||
                             (!is.null(depth) &&
                              curDepth == max(depth))
                if (drawLabel)
                    labelVP(vp$parent, col[colIndex])
            }
        }
        lapply(vp$children, drawVP, curDepth + 1, depth, col, fill, label)
        upViewport()
    }
}

# Draw all viewports in same viewport
showVP <- function(vp, newpage, cvpt, depth, col, fill,
                   label) {
    # Work off a vpTree, so convert vp if it's a vpPath
    showingPath <- inherits(vp, "vpPath")
    if (showingPath) {
        path <- vp
        # If we've started a new page, we'll need the old
        # viewport tree to navigate within
        if (newpage) {
            pushViewport(cvpt)
            # "-1" for "ROOT"
            upViewport(depth(cvpt) - 1)
        }
        downViewport(path)
        vp <- current.vpTree(all=FALSE)
        upViewport(1)
    }
    drawVP(vp, 1, depth, col, fill, label)
    if (showingPath)
        # "-1" because we went down the path then back up 1 originally
        upViewport(depth(path) - 1)
    invisible()
}

# Convert a "viewport" to a set of vpPaths
leafPaths <- function(vp) {
    UseMethod("leafPaths")
}

leafPaths.viewport <- function(vp) {
    if (vp$name == "ROOT")
        NULL
    else
        vp$name
}

leafPaths.vpList <- function(vp) {
    unlist(lapply(vp, leafPaths))
}

leafPaths.vpStack <- function(vp) {
    pathList <- lapply(vp, leafPaths)
    paste(unlist(pathList),
          sep=.grid.pathSep)
}

leafPaths.vpTree <- function(vp) {
    if (is.null(vp$children)) {
        if (vp$parent$name == "ROOT")
            NULL
        else
            vp$parent$name
    } else {
        pathList <- lapply(vp$children, leafPaths)
        if (vp$parent$name == "ROOT") {
            unlist(pathList)
        } else {
            paste(vp$parent$name,
                  unlist(pathList),
                  sep=.grid.pathSep)
        }
    }
}

# Draw a vpPath
drawPath <- function(path, depth, col, fill, label) {
    n <- depth(path)
    for (i in 1:n) {
        downViewport(path[i])
        if (is.null(depth) || i %in% depth) {
            colIndex <- (i - 1) %% length(col) + 1
            fillIndex <- (i - 1) %% length(fill) + 1
            grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
            if (label) {
                if (is.null(depth))
                    drawLabel <- i == n
                else
                    drawLabel <- i == min(n, max(depth))
                if (drawLabel)
                    labelVP(current.viewport(), col[colIndex])
            }
        }
    }
    upViewport(n)
}

# Draw each leaf in separate viewports
# FIXME: allow control over number of rows and cols
# NOTE: this does NOT leave its viewports hanging around after
showVPmatrix <- function(vp, cvpt, depth, col, fill,
                         label, # Only the leaf viewports are labelled
                         nrow, ncol) {
    # Work off a vpPath, so convert vp if it's a "viewport"
    if (is.viewport(vp)) {
        paths <- leafPaths(vp)
    } else {
        # Should not happen
        stop("How did we get here?")
    }
    firstPath <- 0
    while (length(paths) - firstPath > 0) {
        if (firstPath > 0)
            grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow, ncol)))
        for (i in 1:nrow) {
            for (j in 1:ncol) {
                theLeaf <- firstPath + (i - 1)*nrow + j
                if (theLeaf <= length(paths)) {
                    thePath <- vpPathDirect(paths[theLeaf])
                    pushViewport(viewport(layout.pos.row=i,
                                          layout.pos.col=j))
                    grid.rect(gp=gpar(col="grey80"))
                    pushViewport(vp)
                    upViewport(depth(vp))
                    drawPath(thePath, depth, col, fill, label)
                    popViewport()
                }
            }
        }
        popViewport()
        firstPath <- firstPath + nrow*ncol
    }
}

showViewport <- function(vp=NULL, recurse=TRUE, depth=NULL,
                         newpage=FALSE, leaves=FALSE,
                         col=rgb(0, 0, 1, .2), fill=rgb(0, 0, 1, .1),
                         label=TRUE, nrow=3, ncol=nrow) {
    cvpt <- current.vpTree()
    if (is.null(vp))
        vp <- cvpt
    if (newpage == FALSE && leaves == TRUE)
        stop("Must start new page if showing leaves separately")
    if (newpage) {
        grid.newpage()
    }
    if (!recurse)
        depth <- 1
    if (leaves) {
        # Special case of showing vpPath (i.e., only one viewport)
        # Ignores nrow & ncol
        if (inherits(vp, "vpPath"))
            showVP(vp, TRUE, cvpt, depth, col, fill, label)
        else
            showVPmatrix(vp, cvpt, depth, col, fill, label, nrow, ncol)
    } else {
        showVP(vp, newpage, cvpt, depth, col, fill, label)
    }
    invisible()
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
            stop("Must specify at least one of 'x' or 'xscale'")
        xscale <- extendrange(xData, f = extension[1L])
    }
    if (is.null(yscale)) {
        if (is.null(yData))
            stop("Must specify at least one of 'y' or 'yscale'")
        yscale <- extendrange(yData, f = extension[2L])
    }
    viewport(xscale = xscale, yscale = yscale, ...)
}
