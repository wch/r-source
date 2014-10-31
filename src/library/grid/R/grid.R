#  File src/library/grid/R/grid.R
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


# FIXME:  all grid functions should check that .grid.started is TRUE
.grid.loaded <- FALSE

push.vp <- function(vp, recording) {
  UseMethod("push.vp")
}

push.vp.default <- function(vp, recording) {
  stop("only valid to push viewports")
}

push.vp.viewport <- function(vp, recording) {
  # Record on the display list
  if (recording)
    record(vp)
  # Store the entire set of gpar settings JUST PRIOR to push
  # We refer to this when calculating the viewport transform
  # We cannot simply rely on parent's gpar because we may be
  # being pushed from within a gTree which has enforced gpar
  # settings (i.e., the gTree$gp is enforced between this viewport
  # and the this viewport's parent$gp)
  vp$parentgpar <- grid.Call(L_getGPar)
  # Enforce gpar settings
  set.gpar(vp$gp)
  # Store the entire set of gpar settings for this viewport
  vp$gpar <- grid.Call(L_getGPar)
  # Pass in the pushedvp structure which will be used to store
  # things like the viewport transformation, parent-child links, ...
  # In C code, a pushedvp object is created, with a call to pushedvp(),
  # for the system to keep track of
  # (it happens in C code so that a "normal" vp gets recorded on the
  #  display list rather than a "pushedvp")
  grid.Call.graphics(L_setviewport, vp, TRUE)
}

# For all but the last viewport, push the
# viewport then pop it
# For the last viewport, just push
push.vp.vpList <- function(vp, recording) {
  push.vp.parallel <- function(vp, recording) {
    push.vp(vp, recording)
    upViewport(depth(vp), recording)
  }
  if (length(vp) == 1)
    push.vp(vp[[1L]], recording)
  else {
    lapply(vp[1L:(length(vp) - 1)], push.vp.parallel, recording)
    push.vp(vp[[length(vp)]], recording)
  }
}

# Push viewports in series
push.vp.vpStack <- function(vp, recording) {
  lapply(vp, push.vp, recording)
}

# Push parent
# Children are a vpList
push.vp.vpTree <- function(vp, recording) {
  # Special case if user has saved the entire vpTree
  # parent will be the ROOT viewport, which we don't want to
  # push (grid ensures it is ALWAYS there)
  if (!(vp$parent$name %in% "ROOT"))
    push.vp(vp$parent, recording)
  push.vp(vp$children, recording)
}

# "push"ing a vpPath is just a downViewport(..., strict=TRUE)
push.vp.vpPath <- function(vp, recording) {
    downViewport(vp, strict=TRUE, recording)
}

push.viewport <- function(..., recording=TRUE) {
    .Defunct("pushViewport")
}

pushViewport <- function(..., recording=TRUE) {
  if (missing(...))
    stop("must specify at least one viewport")
  else {
    vps <- list(...)
    lapply(vps, push.vp, recording)
  }
  invisible()
}

# Helper functions called from C
no.children <- function(children) {
  length(ls(children, all.names=TRUE)) == 0
}

child.exists <- function(name, children) {
  exists(name, envir=children, inherits=FALSE)
}

child.list <- function(children) {
  ls(children, all.names=TRUE)
}

pathMatch <- function(path, pathsofar, strict) {
  if (is.null(pathsofar))
    is.null(path)
  else {
    pattern <- paste0(if(strict) "^", path, "$")
    grepl(pattern, pathsofar)
  }
}

growPath <- function(pathsofar, name) {
  paste(pathsofar, name, sep=.grid.pathSep)
}

# Rather than pushing a new viewport, navigate down to one that has
# already been pushed
downViewport <- function(name, strict=FALSE, recording=TRUE) {
  UseMethod("downViewport")
}

# For interactive use, allow user to specify
# vpPath directly (i.e., w/o calling vpPath)
downViewport.default <- function(name, strict=FALSE, recording=TRUE) {
  name <- as.character(name)
  downViewport(vpPath(name), strict, recording=recording)
}

# Build vpPath from one (pushed) viewport up to another (pushed) viewport
# 'anc' is assumed to be an ancestor of 'desc'
# 'depth' is the depth that the final depth should have
buildPath <- function(desc, anc, depth) {
    path <- desc$name
    while (!identical(desc$parent, anc)) {
        if (is.null(desc$parent))
            stop("Down viewport failed to record on display list")
        desc <- desc$parent
        path <- c(desc$name, path)
    }
    result <- vpPath(path)
    if (depth(result) != depth)
        warning("Down viewport incorrectly recorded on display list")
    result
}

downViewport.vpPath <- function(name, strict=FALSE, recording=TRUE) {
    start <- grid.Call(L_currentViewport)
    if (name$n == 1)
        result <- grid.Call.graphics(L_downviewport, name$name, strict)
    else
        result <- grid.Call.graphics(L_downvppath,
                                     name$path, name$name, strict)
    # If the downViewport() fails, there is an error in C code
    # so none of the following code will be run

    # Enforce the gpar settings for the viewport
    pvp <- grid.Call(L_currentViewport)
    # Do not call set.gpar because set.gpar accumulates cex
    grid.Call.graphics(L_setGPar, pvp$gpar)
    # Record the viewport operation
    # ... including the depth navigated down
    if (recording) {
        attr(name, "depth") <- result
        # Record the strict path down
        path <- buildPath(pvp, start, result)
        record(path)
    }
    invisible(result)
}

# Similar to down.viewport() except it starts searching from the
# top-level viewport, so the result may be "up" or even "across"
# the current viewport tree
seekViewport <- function(name, recording=TRUE) {
  # up to the top-level
  upViewport(0, recording=recording)
  downViewport(name, recording=recording)
}

# Depth of the current viewport
vpDepth <- function() {
  pvp <- grid.Call(L_currentViewport)
  count <- 0
  while (!is.null(pvp$parent)) {
    pvp <- pvp$parent
    count <- count + 1
  }
  count
}

pop.viewport <- function(n=1, recording=TRUE) {
    .Defunct("popViewport")
}

popViewport <- function(n=1, recording=TRUE) {
  if (n < 0)
    stop("must pop at least one viewport")
  if (n == 0)
    n <- vpDepth()
  if (n > 0) {
    grid.Call.graphics(L_unsetviewport, as.integer(n))
    # Record on the display list
    if (recording) {
      class(n) <- "pop"
      record(n)
    }
  }
  invisible()
}

# Rather than removing the viewport from the viewport stack (tree),
# simply navigate up, leaving pushed viewports in place.
upViewport <- function(n=1, recording=TRUE) {
  if (n < 0)
    stop("must navigate up at least one viewport")
  if (n == 0) {
    n <- vpDepth()
    upPath <- current.vpPath()
  }
  if (n > 0) {
    path <- current.vpPath()
    upPath <- path[(depth(path) - n + 1):depth(path)]
    grid.Call.graphics(L_upviewport, as.integer(n))
    # Record on the display list
    if (recording) {
      class(n) <- "up"
      record(n)
    }
  }
  invisible(upPath)
}

# Return the full vpPath to the current viewport
current.vpPath <- function() {
  names <- NULL
  pvp <- grid.Call(L_currentViewport)
  while (!rootVP(pvp)) {
    names <- c(names, pvp$name)
    pvp <- pvp$parent
  }
  if (!is.null(names))
    vpPathFromVector(rev(names))
  else
    names
}

# Function to obtain the current viewport
current.viewport <- function() {
    # The system stores a pushedvp;  the user should only
    # ever see normal viewports, so convert.
    vpFromPushedvp(grid.Call(L_currentViewport))
}

# Return the parent of the current viewport
# (could be NULL)
current.parent <- function(n=1) {
    if (n < 1)
        stop("Invalid number of generations")
    vp <- grid.Call(L_currentViewport)
    generation <- 1
    while (generation <= n) {
        if (is.null(vp))
            stop("Invalid number of generations")
        vp <- vp$parent
        generation <- generation + 1
    }
    if (!is.null(vp))
        vpFromPushedvp(vp)
    else
        vp
}

vpListFromNode <- function(node) {
  childnames <- ls(node$children, all.names=TRUE)
  n <- length(childnames)
  children <- vector("list", n)
  index <- 1
  for (i in childnames) {
    children[[index]] <- vpTreeFromNode(get(i, envir=node$children))
    index <- index + 1
  }
  vpListFromList(children)
}

vpTreeFromNode <- function(node) {
  # If no children then just return viewport
  if (length(ls(node$children, all.names=TRUE)) == 0)
    vpFromPushedvp(node)
  # Otherwise return vpTree
  else
    vpTree(vpFromPushedvp(node),
           vpListFromNode(node))
}

# Obtain the current viewport tree
# Either from the current location in the tree down
# or ALL of the tree
current.vpTree <- function(all=TRUE) {
  cpvp <- grid.Call(L_currentViewport)
  moving <- all && vpDepth() > 0
  if (moving) {
    savedname <- cpvp$name
    upViewport(0, recording=FALSE)
    cpvp <- grid.Call(L_currentViewport)
  }
  tree <- vpTreeFromNode(cpvp)
  if (moving) {
    downViewport(savedname, recording=FALSE)
  }
  tree
}

current.transform <- function() {
    grid.Call(L_currentViewport)$trans
}

current.rotation <- function() {
    grid.Call(L_currentViewport)$rotation
}

# Call this function if you want the graphics device erased or moved
# on to a new page.  High-level plotting functions should call this.
# NOTE however, that if you write a function which calls grid.newpage,
# you should provide an argument to allow people to turn it off
# so that they can use your function within a parent viewport
# (rather than the whole device) if they want to.
grid.newpage <- function(recording=TRUE) {
    for (fun in getHook("before.grid.newpage"))  {
        if(is.character(fun)) fun <- get(fun)
        try(fun())
    }
    # NOTE that we do NOT do grid.Call here because we have to do
    # things slightly differently if grid.newpage is the first grid operation
    # on a new device
    .Call(L_newpagerecording)
    .Call(L_newpage)
    .Call(L_initGPar)
    .Call(L_initViewportStack)
    if (recording) {
        .Call(L_initDisplayList)
        grDevices:::recordPalette()
        for (fun in getHook("grid.newpage"))  {
            if(is.character(fun)) fun <- get(fun)
            try(fun())
        }
    }
    invisible()
}

###########
# DISPLAY LIST FUNCTIONS
###########

# Keep a list of all drawing operations (since last grid.newpage()) so
# that we can redraw upon edit.

inc.display.list <- function() {
  display.list <- grid.Call(L_getDisplayList)
  dl.index <- grid.Call(L_getDLindex)
  dl.index <- dl.index + 1
  n <- length(display.list)
  # The " - 1" below is because dl.index is now stored internally
  # so is a C-style zero-based index rather than an R-style
  # 1-based index
  if (dl.index > (n - 1)) {
    temp <- display.list
    display.list <- vector("list", n + 100L)
    display.list[1L:n] <- temp
  }
  grid.Call(L_setDisplayList, display.list)
  grid.Call(L_setDLindex, as.integer(dl.index))
}

# This will either ...
#   (i) turn on AND INITIALISE the display list or ...
#   (ii) turn off AND ERASE the display list
grid.display.list <- function(on=TRUE) {
  grid.Call(L_setDLon, as.logical(on))
  if (on) {
    grid.Call(L_setDisplayList, vector("list", 100L))
    grid.Call(L_setDLindex, 0L)
  }
  else
    grid.Call(L_setDisplayList, NULL)
}

record <- function(x) {
  if (grid.Call(L_getDLon))
    UseMethod("record")
}

# When there is a pop.viewport, the number of viewports popped
# gets put on the display list
record.default <- function(x) {
  if (!is.numeric(x))
    stop("invalid object inserted on the display list")
  grid.Call(L_setDLelt, x)
  inc.display.list()
}

record.grob <- function(x) {
  grid.Call(L_setDLelt, x)
  inc.display.list()
}

record.viewport <- function(x) {
  grid.Call(L_setDLelt, x)
  inc.display.list()
}

record.vpPath <- function(x) {
  grid.Call(L_setDLelt, x)
  inc.display.list()
}

# This controls whether grid is using the graphics engine's display list
engine.display.list <- function(on=TRUE) {
  grid.Call(L_setEngineDLon, as.logical(on))
}

# Rerun the grid DL
grid.refresh <- function() {
  draw.all()
}

# Call a function on each element of the grid display list
# AND replace the element with the result
# This is blood-curdlingly dangerous for the state of the
# display list
# Two token efforts at safety are made:
#   - generate all of the new elements first THEN assign them all
#     (so if there is an error in generating any one element
#      you don't end up with a trashed display list)
#   - check that the new element is either NULL or the same
#     class as the element it is replacing
grid.DLapply <- function(FUN, ...) {
    FUN <- match.fun(FUN)
    # Traverse DL and do something to each entry
#    gridDL <- grid.Call(L_getDisplayList)
    gridDLindex <- grid.Call(L_getDLindex)
    newDL <- vector("list", gridDLindex)
    for (i in 1:(gridDLindex - 1)) {
        elt <- grid.Call(L_getDLelt, i)
        newElt <- FUN(elt, ...)
        if (!(is.null(newElt) || inherits(newElt, class(elt))))
            stop("invalid modification of the display list")
        newDL[[i]] <- newElt
    }
    for (i in 1:(gridDLindex - 1)) {
        grid.Call(L_setDLindex, i)
        grid.Call(L_setDLelt, newDL[[i]])
    }
    grid.Call(L_setDLindex, gridDLindex)
}

# Wrapper for .Call and .Call.graphics
# Used to make sure that grid-specific initialisation occurs just before
# the first grid graphics output OR the first querying of grid state
# (on the current device)
# The general rule is you should use these rather than .Call or
# .Call.graphics unless you have a good reason and you know what
# you are doing -- this will be a bit of overkill, but is for safety
grid.Call <- function(fnname, ...) {
  .Call(L_gridDirty)
  .Call(dontCheck(fnname), ...)  # skip code analysis checks, keep runtime checks
}

grid.Call.graphics <- function(fnname, ...) {
  # Only record graphics operations on the graphics engine's display
  # list if the engineDLon flag is set
  engineDLon <- grid.Call(L_getEngineDLon)
  if (engineDLon) {
    # NOTE that we need a .Call.graphics("L_gridDirty") so that
    # the the first thing on the engine display list is a dirty
    # operation;  this is necessary in case the display list is
    # played on another device (e.g., via replayPlot() or dev.copy())
    .Call.graphics(L_gridDirty)
    result <- .Call.graphics(dontCheck(fnname), ...)
  } else {
    .Call(L_gridDirty)
    result <- .Call(dontCheck(fnname), ...)
  }
  result
}

# A call to recordGraphics() outside of [pre|post]drawDetails methods
# will not record the expr on the grid DL.
# If a user REALLY wants to call recordGraphics(), they should use
# grid.record() instead
drawDetails.recordedGrob <- function(x, recording) {
  eval(x$expr, x$list, getNamespace("grid"))
}

grid.record <- function(expr, list,
                        name=NULL, gp=NULL, vp=NULL) {
  grid.draw(grob(expr=substitute(expr), list=list,
                 name=name, gp=gp, vp=vp, cl="recordedGrob"))
}

recordGrob <- function(expr, list,
                       name=NULL, gp=NULL, vp=NULL) {
  grob(expr=substitute(expr), list=list,
       name=name, gp=gp, vp=vp, cl="recordedGrob")
}

# Must only generate a grob, not modify drawing context
makeContent.delayedgrob <- function(x) {
    grob <- eval(x$expr, x$list, getNamespace("grid"))
    if (is.grob(grob)) {
        children <- gList(grob)
    } else if (is.gList(grob)) {
        children <- grob
    } else {
        stop("'expr' must return a grob or gList")
    }
    x <- setChildren(x, children)
    x
}

grid.delay <- function(expr, list,
                       name=NULL, gp=NULL, vp=NULL) {
    grid.draw(gTree(expr=substitute(expr), list=list,
                    name=name, gp=gp, vp=vp, cl="delayedgrob"))
}

delayGrob <- function(expr, list,
                      name=NULL, gp=NULL, vp=NULL) {
    gTree(expr=substitute(expr), list=list,
          name=name, gp=gp, vp=vp, cl="delayedgrob")
}

