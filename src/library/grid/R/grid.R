
# FIXME:  all grid functions should check that .grid.started is TRUE
.grid.loaded <- FALSE

push.vp <- function(vp, recording) {
  UseMethod("push.vp")
}

push.vp.default <- function(vp, recording) {
  stop("Only valid to push viewports")
}

push.vp.viewport <- function(vp, recording) {
  # Record on the display list
  if (recording)
    record(vp)
  # Enforce gpar settings
  set.gpar(vp$gp)
  # Create a pushedvp object for the system to keep track of
  pvp <- pushedvp(vp)
  # Store the entire set of gpar settings for this viewport
  pvp$gpar <- grid.Call("L_getGPar")
  # Pass in the pushedvp structure which will be used to store
  # things like the viewport transformation, parent-child links, ...
  grid.Call.graphics("L_setviewport", pvp, TRUE)
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
    push.vp(vp[[1]], recording)
  else {
    lapply(vp[1:(length(vp) - 1)], push.vp.parallel, recording)
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
  if (!(vp$paren$name %in% "ROOT"))
    push.vp(vp$parent, recording)
  push.vp(vp$children, recording)
}

push.viewport <- function(..., recording=TRUE) {
  .Deprecated("pushViewport")
  pushViewport(..., recording=recording)
}

pushViewport <- function(..., recording=TRUE) {
  if (missing(...))
    stop("Must specify at least one viewport")
  else {
    vps <- list(...)
    lapply(vps, push.vp, recording)
  }
  current.viewport()
}

# Helper functions called from C
no.children <- function(children) {
  length(ls(env=children)) == 0
}

child.exists <- function(name, children) {
  exists(name, env=children, inherits=FALSE)
}

child.list <- function(children) {
  ls(env=children)
}

# Rather than pushing a new viewport, navigate down to one that has
# already been pushed
downViewport <- function(name, recording=TRUE) {
  if (grid.Call.graphics("L_downviewport", name)) {
    if (recording) {
      class(name) <- "down"
      record(name)
    } 
  } else {
    stop(paste("Viewport", name, "is not currently pushed"))
  }
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
  pvp <- grid.Call("L_currentViewport")
  count <- 0
  while (!is.null(pvp$parent)) {
    pvp <- pvp$parent
    count <- count + 1
  }
  count
}

pop.vp <- function(last.one, recording) {
  pvp <- grid.Call("L_currentViewport")
  # Fail if trying to pop top-level viewport
  if (is.null(pvp$parent))
    stop("Illegal to pop top-level viewport")
  # Assert the gpar settings of the parent (which is about to become "current")
  pgpar <- pvp$parent$gpar
  class(pgpar) <- "gpar"
  set.gpar(pgpar)
  # Allow for recalculation of viewport transform if necessary
  # and do things like updating parent/children slots in
  # stored pushedvps
  grid.Call.graphics("L_unsetviewport", last.one)
}

pop.viewport <- function(n, recording=TRUE) {
  .Deprecated("popViewport")
  popViewport(n, recording=recording)
}

popViewport <- function(n=1, recording=TRUE) {
  if (n < 0)
    stop("Must pop at least one viewport")
  if (n == 0)
    n <- vpDepth()
  if (n > 0) {
    for (i in 1:n)
      pop.vp(i==n, recording)
    # Record on the display list
    if (recording) {
      class(n) <- "pop"
      record(n)
    }
  }
}

up.vp <- function(last.one, recording) {
  pvp <- grid.Call("L_currentViewport")
  # Fail if trying to up top-level viewport
  if (is.null(pvp$parent))
    stop("Illegal to navigate up past top-level viewport")
  # Assert the gpar settings of the parent (which is about to become "current")
  pgpar <- pvp$parent$gpar
  class(pgpar) <- "gpar"
  set.gpar(pgpar)
  # Allow for recalculation of viewport transform if necessary
  grid.Call.graphics("L_upviewport", last.one)
}

# Rather than removing the viewport from the viewport stack (tree),
# simply navigate up, leaving pushed viewports in place.
upViewport <- function(n=1, recording=TRUE) {
  if (n < 0)
    stop("Must navigate up at least one viewport")
  if (n == 0) 
    n <- vpDepth()
  if (n > 0) {
    for (i in 1:n)
      up.vp(i==n, recording)
    # Record on the display list
    if (recording) {
      class(n) <- "up"
      record(n)
    }
  }
}
                        
# Function to obtain the current viewport
current.viewport <- function(vp=NULL) {
  if (is.null(vp)) 
    # The system stores a pushedvp;  the user should only
    # ever see normal viewports, so convert.
    vpFromPushedvp(grid.Call("L_currentViewport"))
  else {
    warning("The vp argument is deprecated")
    vp
  }
}

vpListFromNode <- function(node) {
  childnames <- ls(env=node$children)
  n <- length(childnames)
  children <- vector("list", n)
  index <- 1
  for (i in childnames) {
    children[[index]] <- vpTreeFromNode(get(i, env=node$children))
    index <- index + 1
  }
  vpListFromList(children)
}

vpTreeFromNode <- function(node) {
  # If no children then just return viewport
  if (length(ls(env=node$children)) == 0)
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
  cpvp <- grid.Call("L_currentViewport")
  if (all && vpDepth() > 0) {
    savedname <- cpvp$name
    upViewport(0)
    cpvp <- grid.Call("L_currentViewport")
  }
  tree <- vpTreeFromNode(cpvp)
  if (all) {
    downViewport(savedname)
  }
  tree
}

current.transform <- function() {
  grid.Call("L_currentViewport")$trans
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

record.down <- function(x) {
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

