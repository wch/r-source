#########
# Generate a gTree from the current display list
#
# Or from an expression
# (recording on to a null graphics device)
#########
rootVP <- function(pvp) {
  match(pvp$name, "ROOT", nomatch=FALSE)
}

# Return the full vpPath to the current viewport
current.vpPath <- function() {
  names <- NULL
  pvp <- grid.Call("L_currentViewport")
  while (!rootVP(pvp)) {
    names <- c(names, pvp$name)
    pvp <- pvp$parent
  }
  if (!is.null(names))
    vpPathFromVector(rev(names))
  else
    names
}

# List the children of the current vp (as a vpList)
current.vpList <- function() {
  cpvp <- grid.Call("L_currentViewport")
  if (length(ls(env=cpvp$children, all.names=TRUE)) == 0)
    NULL
  else
    vpListFromNode(cpvp)
}

current.vpNames <-function() {
  ls(env=grid.Call("L_currentViewport")$children)
}

grabWarn <- function(msg, certain, wrapFix) {
  warning(msg, " (grab ",
          if (certain) gettext("WILL not be faithful")
          else gettext("MAY not be faithful"),
          if (wrapFix) "; try wrap=TRUE)" else ")",
          call.=FALSE, domain = NA)
}

warn1 <- function(msg, wrapFix) {
  grabWarn(msg, TRUE, wrapFix)
}

warn2 <- function(msg, wrapFix) {
  grabWarn(msg, FALSE, wrapFix)
}

# vp might be a viewport, or a vpList, or a vpStack, or a vpTree
vpExists <- function(vp) {
  UseMethod("vpExists")
}

vpExists.viewport <- function(vp) {
  vp$name %in% ls(env=.Call("L_currentViewport")$children)
}

vpExists.vpStack <- function(vp) {
  vpExists(vp[[1]])
}

vpExists.vpList <- function(vp) {
  any(sapply(vp, vpExists, simplify=TRUE))
}

vpExists.vpTree <- function(vp) {
  vpExists(vp$parent)
}

wrap <- function(x) {
  UseMethod("wrap")
}

wrap.default <- function(x) {
  if (!is.null(x))
    stop("Invalid display list element")
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

wrap.down <- function(x) {
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
  dl.index <- grid.Call("L_getDLindex")
  if (dl.index > 1) {
    if (warn > 0) {
      names <- getNames()
      # Check for overwriting existing grob
      if (length(unique(names)) != length(names))
        warn1(gettext("grob(s) overwritten"), TRUE)
    }
    grid.newpage(recording=FALSE)
    # Start at 2 because first element is viewport[ROOT]
    for (i in 2:dl.index) {
      # Do all of this as a big ifelse rather than
      # dispatching to a function call per element because
      # we need to work with whole DL at times, not
      # just individual elements
      elt <- grid.Call("L_getDLelt", as.integer(i - 1))
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
              warn2("viewport overwritten", FALSE)
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
              warn2("grob pushed viewports and did not pop/up them", TRUE)
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
            warn2("viewport overwritten", FALSE)
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
  # Start a new null device
  .Call("L_nullDevice", PACKAGE="grDevices")
  # If something goes wrong, want to revert to the current device
  on.exit(dev.off())
  # Run the graphics code in expr
  eval(substitute(expr))
  # Grab the DL on the new device
  grabDL(warn, wrap, ...)
}
