
# FIXME:  all grid functions should check that .grid.started is TRUE
.grid.loaded <- FALSE

# Define a convenience function that is easy to call from C code
grid.top.level.vp <- function() {
  viewport(clip=TRUE)
}

push.vp <- function(vps, index, len, recording) {
  vp <- vps[[index]]
  if (is.null(vp))
    stop("Illegal to push NULL viewport")
  # Record on the display list
  if (recording)
    record(vp)
  # Enforce gpar settings
  set.gpar(vp$gp)
  # Later, we will query the viewport to ask "what were the gpar
  # settings when you were drawn".  This is NOT the same as asking
  # the viewport for its gpar settings because the viewport may only
  # specify some gpar values.  So we record the default settings
  # we will need to know about
  vp$cur.fontfamily <- get.gpar("fontfamily")
  vp$cur.font <- get.gpar("font")
  vp$cur.fontsize <- get.gpar("fontsize")
  vp$cur.lineheight <- get.gpar("lineheight")
  vp$cur.cex <- get.gpar("cex")
  # Calculate viewport transform
  # NOTE that we will have modified "vp" within L_setviewport
  # to record the current transformation and layout
  grid.Call.graphics("L_setviewport", vp, TRUE)
  # Push further viewports if required
  if (index < len)
    push.vp(vps, index+1, len, recording)
}

push.viewport <- function(..., recording=TRUE) {
  if (missing(...))
    stop("Must specify at least one viewport")
  else {
    vps <- list(...)
    nvp <- length(vps)
    push.vp(vps, 1, nvp, recording)
  }
}

pop.vp <- function(last.one, recording) {
  vp <- grid.Call("L_currentViewport")
  # Fail if trying to pop top-level viewport
  if (is.null(vp$parent))
    stop("Illegal to pop top-level viewport")
  # Unset gpar settings
  unset.gpar(vp$gp)
  # Allow for recalculation of viewport transform if necessary
  grid.Call.graphics("L_unsetviewport", last.one)
}

pop.viewport <- function(n=1, recording=TRUE) {
  if (n < 1)
    stop("Must pop at least one viewport")
  else {
    for (i in 1:n)
      pop.vp(i==n, recording)
    # Record on the display list
    if (recording)
      record(n)
  }
}

# Function to obtain the current viewport
current.viewport <- function(vp=NULL) {
  if (is.null(vp))
    grid.Call("L_currentViewport")
  else {
    warning("The vp argument is going to be deprecated")
    vp
  }
}

# Call this function if you want the graphics device erased or moved
# on to a new page.  High-level plotting functions should call this.
# NOTE however, that if you write a function which calls grid.newpage,
# you should provide an argument to allow people to turn it off
# so that they can use your function within a parent viewport
# (rather than the whole device) if they want to.
grid.newpage <- function(recording=TRUE) {
  # NOTE that we do NOT do grid.Call here because we have to do
  # things slightly differently if grid.newpage is the first grid operation
  # on a new device
  .Call("L_newpagerecording", par("ask"), PACKAGE="grid")
  .Call("L_newpage", PACKAGE="grid")
  .Call("L_initGPar", PACKAGE="grid")
  .Call("L_initViewportStack", PACKAGE="grid")
  if (recording)
    .Call("L_initDisplayList", PACKAGE="grid")
}

###########
# DISPLAY LIST FUNCTIONS
###########

# Keep a list of all drawing operations (since last grid.newpage()) so
# that we can redraw upon edit.

inc.display.list <- function() {
  display.list <- grid.Call("L_getDisplayList")
  dl.index <- grid.Call("L_getDLindex")
  dl.index <- dl.index + 1
  n <- length(display.list)
  # The " - 1" below is because dl.index is now stored internally
  # so is a C-style zero-based index rather than an R-style
  # 1-based index
  if (dl.index > (n - 1)) {
    temp <- display.list
    display.list <- vector("list", n+100)
    display.list[1:n] <- temp
  }
  grid.Call("L_setDisplayList", display.list)
  grid.Call("L_setDLindex", as.integer(dl.index))
}

# This will either ...
#   (i) turn on AND INITIALISE the display list or ...
#   (ii) turn off AND ERASE the display list
grid.display.list <- function(on=TRUE) {
  grid.Call("L_setDLon", as.logical(on))
  if (on) {
    grid.Call("L_setDisplayList", vector("list", 100))
    grid.Call("L_setDLindex", as.integer(0))
  }
  else
    grid.Call("L_setDisplayList", NULL)
}

record <- function(x) {
  if (grid.Call("L_getDLon"))
    UseMethod("record")
}

# When there is a pop.viewport, the number of viewports popped
# gets put on the display list
record.default <- function(x) {
  if (!is.numeric(x))
    stop("Invalid object inserted on the display list")
  grid.Call("L_setDLelt", x)
  inc.display.list()
}

record.grob <- function(x) {
  grid.Call("L_setDLelt", x)
  inc.display.list()
}

record.viewport <- function(x) {
  grid.Call("L_setDLelt", x)
  inc.display.list()
}

# Wrapper for .Call and .Call.graphics
# Used to make sure that grid-specific initialisation occurs just before
# the first grid graphics output OR the first querying of grid state
# (on the current device)
# The general rule is you should use these rather than .Call or
# .Call.graphics unless you have a good reason and you know what
# you are doing -- this will be a bit of overkill, but is for safety
grid.Call <- function(fnname, ...) {
  .Call("L_gridDirty", PACKAGE="grid")
  .Call(fnname, ..., PACKAGE="grid")
}

grid.Call.graphics <- function(fnname, ...) {
  .Call.graphics("L_gridDirty", PACKAGE="grid")
  .Call.graphics(fnname, ..., PACKAGE="grid")
}

