
# Define a convenience function that is easy to call from C code
grid.top.level.vp <- function() {
  pushedvp(viewport(clip=TRUE, name="ROOT"))
}

# An accessor for getting at the grid global state structure
# to make debugging easier for me;  all I have to type is grid:::STATE()
STATE <- function() {
  get(".GRID.STATE", envir=.GridEvalEnv)
}

is.odd <- function(x) {
  x %% 2
}

is.even <- function(x) {
  !is.odd(x)
}

grid.pretty <- function(range) {
  if (!is.numeric(range))
    stop("range must be numeric")
  .Call("L_pretty", range, PACKAGE="grid")
}

#########
# Generate a gTree from the current display list
#########
rootVP <- function(pvp) {
  match(pvp$name, "ROOT", nomatch=FALSE)
}

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

current.vpList <- function() {
  cpvp <- grid.Call("L_currentViewport")
  vpListFromNode(cpvp)
}

grid.grab <- function(...) {
  gList <- NULL
  dl.index <- grid.Call("L_getDLindex")
  if (dl.index > 1) {
    grid.newpage(recording=FALSE)
    # Start at 2 because first element is viewport[ROOT]
    for (i in 2:dl.index) {
      dlElt <- grid.Call("L_getDLelt", as.integer(i - 1))
      if (inherits(dlElt, "pop"))
        upViewport(dlElt, recording=FALSE)
      else {
        if (is.grob(dlElt)) {
          # Enforce grob$vp now and set grob$vp to NULL
          # Will be replaced later with full vpPath
          tempvp <- dlElt$vp
          if (!is.null(tempvp))
            tempdepth <- depth(tempvp)
          grid.draw(tempvp)
          dlElt$vp <- NULL
        }
        grid.draw(dlElt, recording=FALSE)
        if (is.grob(dlElt)) {
          dlElt$vp <- current.vpPath()
          if (!is.null(tempvp))
            upViewport(tempdepth)
          gList <- addToGList(dlElt, gList)
        }
      }
    }
    # Go to top level
    upViewport(0, recording=FALSE)
    gTree(children=gList, childrenvp=current.vpList(), ...)
  } else {
    stop("Display list is empty")
  }
}
