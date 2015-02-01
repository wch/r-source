#  File src/library/grid/R/grab.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

#########
# Generate a gTree from the current display list
#
# Or from an expression
# (recording on to a null graphics device)
#########
rootVP <- function(pvp) {
  match(pvp$name, "ROOT", nomatch=FALSE)
}

# List the children of the current vp (as a vpList)
current.vpList <- function() {
  cpvp <- grid.Call(L_currentViewport)
  if (no.children(cpvp$children))
    NULL
  else
    vpListFromNode(cpvp)
}

current.vpNames <- function() {
  ls(grid.Call(L_currentViewport)$children)
}

# vp might be a viewport, or a vpList, or a vpStack, or a vpTree
vpExists <- function(vp) {
  UseMethod("vpExists")
}

vpExists.viewport <- function(vp) {
  exists(vp$name, .Call(L_currentViewport)$children)
}

vpExists.vpStack <- function(vp) {
  vpExists(vp[[1L]])
}

vpExists.vpList <- function(vp) {
  any(vapply(vp, vpExists, logical(1L), simplify=TRUE))
}

vpExists.vpTree <- function(vp) {
  vpExists(vp$parent)
}

# Handle vpPaths in a vpStack or vpTree
# Not a problem to downViewport() to a viewport that already exists
vpExists.vpPath <- function(vp) {
    FALSE
}

wrap <- function(x) {
  UseMethod("wrap")
}

wrap.default <- function(x) {
  if (!is.null(x))
    stop("invalid display list element")
  NULL
}

wrap.grob <- function(x) {
  x
}

wrap.viewport <- function(x) {
  recordGrob(pushViewport(vp), list(vp=x))
}

wrap.pop <- function(x) {
  recordGrob(popViewport(n), list(n=x))
}

wrap.up <- function(x) {
  recordGrob(upViewport(n), list(n=x))
}

wrap.vpPath <- function(x) {
  recordGrob(downViewport(path), list(path=x))
}

# Grab the display list on the current device
# ... are passed to gTree
# If warn is 0, issue no warnings
# If warn is 1, issue warnings about situations that are definitely
#   NOT captured correctly (e.g., reuse of top-level grob name)
# If warn is 2, issue warnings about situations that
#   MAY not get captured correctly (e.g., top-level downViewport)
# If wrap is TRUE, grab will wrap all pushes and grobs
#   in a gTree
grabDL <- function(warn, wrap, ...) {
  gList <- NULL
  dl.index <- grid.Call(L_getDLindex)
  if (dl.index > 1) {
    if (warn > 0) {
      names <- getNames()
      # Check for overwriting existing grob
      if (length(unique(names)) != length(names))
        warning("one of more grobs overwritten (grab WILL not be faithful; try 'wrap = TRUE')")
    }
    grid.newpage(recording=FALSE)
    # Start at 2 because first element is viewport[ROOT]
    for (i in 2:dl.index) {
      # Do all of this as a big ifelse rather than
      # dispatching to a function call per element because
      # we need to work with whole DL at times, not
      # just individual elements
      elt <- grid.Call(L_getDLelt, as.integer(i - 1))
      if (wrap)
        gList <- addToGList(wrap(elt), gList)
      else {

        ###########
        # grabGrob
        ###########
        if (inherits(elt, "grob")) {
          # Enforce grob$vp now and set grob$vp to NULL
          # Will be replaced later with full vpPath
          tempvp <- elt$vp
          if (warn > 1) {
            # Check to see if about to push a viewport
            # with existing viewport name
            if (inherits(tempvp, "viewport") &&
                vpExists(tempvp))
              warning("viewport overwritten (grab MAY not be faithful)")
          }
          if (!is.null(tempvp))
            tempdepth <- depth(tempvp)
          grid.draw(tempvp, recording=FALSE)
          # vpPath after grob$vp slot has been pushed
          # Has to be recorded here in case grob drawing
          # pushes (and does not pop) more viewports
          drawPath <- current.vpPath()
          elt$vp <- NULL
          grid.draw(elt, recording=FALSE)
          if (warn > 1) {
            # Compare new vpPath
            # If not same, the grob has pushed some viewports
            # and not popped or upped them
            pathSame <- TRUE
            if (!(is.null(drawPath) && is.null(current.vpPath()))) {
              if (is.null(drawPath))
                pathSame = FALSE
              else if (is.null(current.vpPath()))
                pathSame = FALSE
              else if (as.character(drawPath) !=
                       as.character(current.vpPath()))
                pathSame = FALSE
            }
            if (!pathSame)
              warning("grob pushed viewports and did not pop/up them (grab MAY not be faithful)")
          }
          elt$vp <- drawPath
          if (!is.null(tempvp))
            upViewport(tempdepth, recording=FALSE)
          gList <- addToGList(elt, gList)
        ###########
        # grabViewport
        ###########
        } else if (inherits(elt, "viewport")) {
          # Includes viewports, vpLists, vpTrees, and vpStacks
          # Check to see if about to push a viewport
          # with existing viewport name
          if (vpExists(elt))
            warning("viewport overwritten (grab MAY not be faithful)")
          grid.draw(elt, recording=FALSE)
        ###########
        # grabPop
        ###########
        } else if (inherits(elt, "pop")) {
          # Replace pop with up
          upViewport(elt, recording=FALSE)

        ###########
        # grabDefault
        ###########
        } else {
          grid.draw(elt, recording=FALSE)
        }
      } # matches if (wrap)
    }
    # Go to top level
    upViewport(0, recording=FALSE)
    gTree(children=gList, childrenvp=current.vpList(), ...)
  } else {
    NULL
  }
}

# expr is ignored if dev is NULL
# otherwise, it should be an expression, like postscript("myfile.ps")
grid.grab <- function(warn=2, wrap=FALSE, ...) {
  grabDL(warn, wrap, ...)
}

grid.grabExpr <- function(expr, warn=2, wrap=FALSE, ...) {
    ## Start an "offline" PDF device for this function
    pdf(file=NULL)
    on.exit(dev.off())
    ## Run the graphics code in expr
    ## Rely on lazy evaluation for correct "timing"
    eval(expr)
    ## Grab the DL on the new device
    grabDL(warn, wrap, ...)
}

#########################
# A different sort of capture ...
# Just grab the screen raster image
#########################

grid.cap <- function() {
    # This does not need recording on the display list
    grid.Call(L_cap)
}


