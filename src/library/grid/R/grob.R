######################################
# Grid graphical primitives
#######################################

# A graphical object is a unique object (i.e., we refer to it by pointer)
# so that it can be edited
# NOTE that cl is the class of the list.struct and "grob" is
# the class of the reference object
# The aim is to have user code only deal with the list.struct
# and hide the handling of pointers
# NOTE also that we stick class "glist" onto the list structure
# so that we can do generic things with them too.
grid.grob <- function(list.struct, cl=NULL, draw=TRUE) {
  class(list.struct) <- c(cl, "glist")
  ptr <- .Call("L_CreateSEXPPtr", list.struct, PACKAGE="grid")
  grob <- list(ptr)
  class(grob) <- "grob"
  if (draw)
    grid.draw(grob)
  invisible(grob)
}

is.grob <- function(x) {
  inherits(x, "grob")
}

get.value <- function(x, child.specs=NULL) {
  UseMethod("get.value")
}

get.value.default <- function(x, child.specs=NULL) {
  if (is.list(x) && length(child.specs) > 0)
      get.value(x[[child.specs[[1]]]], child.specs[-1])
  else
    x
}

get.value.grob <- function(x, child.specs=NULL) {
  # Remove check when have NAMESPACE (?)
  if (!is.grob(x))
    stop("Cannot get value of non-grob")
  result <- .Call("L_GetSEXPPtr", x[[1]], PACKAGE="grid")
  if (length(child.specs) > 0) 
    result <- get.value(result[[child.specs[[1]]]],
                        child.specs[-1])
  result
}

# Unwrap a list.struct from within a grob external pointer
grid.get <- function(grob, ...) {
  if (!is.grob(grob))
    stop("Cannot get value of non-grob")
  get.value.grob(grob, list(...))
}

# FIXME:  Replace with "<-.grob" method ?
set.value.grob <- function(grob, child.specs, list.struct) {
  ncs <- length(child.specs)  
  if (ncs == 0)
    target <- grob
  else
    target <- get.value.grob(grob, child.specs[-ncs])[[child.specs[[ncs]]]]
  .Call("L_SetSEXPPtr", target[[1]], list.struct, PACKAGE="grid")
}

# Wrap a list.struct within a grob external pointer
# Destructively set value of a grob
grid.set <- function(grob, ...) {
  if (!is.grob(grob))
    stop("Cannot set value of non-grob")
  args <- list(...)
  nargs <- length(args)
  if (nargs == 0)
    stop("No list.struct value specified")
  set.value.grob(grob, args[-nargs], args[[nargs]])
}

# Use this function to produce a list of new.values for grid.edit()
grid.prop.list <- function(...) {
  result <- list(...)
  class(result) <- "prop.list"
  result
}

# The ... part consists of zero or more child.specs, plus a single
# new.value or a list of new.values
grid.edit <- function(grob, ..., redraw=TRUE) {
  # If grob is NULL, do nothing, but don't give an error
  # This allows grobs to have NULL components
  if (!is.null(grob)) {
    if (!inherits(grob, "grob"))
      stop("Cannot edit value of non-grob")
    args <- list(...)
    nargs <- length(args)
    if (nargs == 0)
      stop("No new value specified")
    new.values <- args[nargs]
    # Handle list of new values
    if (inherits(new.values[[1]], "prop.list")) 
      new.values <- new.values[[1]]
    # Make sure that when grid.edit is called again from within
    # an edit.details method, that the new.values is a prop.list
    class(new.values) <- "prop.list"
    # If there are no new.values, just do nothing
    # This is possible, e.g., axis consumes at= and passes empty
    # new.values to axis$major etc
    if (length(new.values) > 0 && !is.null(names(new.values))) {
      child.specs <- args[-nargs]
      list.struct <- get.value.grob(grob, child.specs)
      slot.names <- names(new.values)
      for (i in 1:length(new.values)) 
        # If there is no slot with the argument name, just ignore that argument
        if (match(slot.names[i], names(list.struct), nomatch=0)) {
          list.struct[[slot.names[i]]] <- new.values[[i]]
          # If the new value was NULL, we have just erased the slot
          # from the list.struct.  Here we put it back.
          # FIXME: there must be a better way to do this !
          if (is.null(new.values[[i]])) {
            cl <- class(list.struct)
            temp <- list(NULL)
            names(temp) <- slot.names[i]
            list.struct <- c(list.struct, temp)
            class(list.struct) <- cl
          }
        }
      # Do any class-specific editing
      list.struct <- editDetails(list.struct, new.values)
      set.value.grob(grob, child.specs, list.struct)
      # FIXME:  This needs to draw ldisplay.list for all devices where
      # grob appears
      if (redraw)
        draw.all()
    }
  }
}

editDetails <- function(x, new.values) {
  UseMethod("editDetails")
}

editDetails.default <- function(x, new.values) {
  # Do nothing BUT return object being edited
  x
}

# Use generic function "draw" rather than generic function "print"
# because want graphics functions to produce graphics output
# without having to be evaluated at the command-line AND without having
# to necessarily produce a single graphical object as the return value
# (i.e., so that simple procedural code can be written just for its
# side-effects).
# For example, so that the following code will draw
# a rectangle AND a line:
#   temp <- function() { grid.lines(); grid.rect() }
#   temp()
# All drawing methods have to extract the grob value at the start and
# record if necessary at the end.  The approach below means that custom
# drawing methods don't have to bother about this;  they just have to
# write a draw.details method
# Assume that all grobs have a slot called "vp" containing a viewport
# and a slot "gpar" containing a gpar
grid.draw <- function(x, recording=TRUE) {
  if (!is.null(x)) {
      list.struct <- get.value(x)
      # automatically push/pop the viewport and set/unset the gpar
      if (!is.null(list.struct$vp))
        push.viewport(list.struct$vp, recording=FALSE)
      if (!is.null(list.struct$gp))
        set.gpar(list.struct$gp)
      # Do any class-specific drawing
      draw.details(list.struct, x, recording)
      if (!is.null(list.struct$gp))
        unset.gpar(list.struct$gp)
      if (!is.null(list.struct$vp))
          pop.viewport(recording=FALSE)
      if (recording)
        record(x)
  }
}

draw.all <- function() {
  grid.newpage(recording=FALSE)
  lapply(grid.Call("L_getDisplayList"), grid.draw, recording=FALSE)
  NULL
}

draw.details <- function(x, x.wrapped, recording) {
  UseMethod("draw.details")
}

# When there is a pop.viewport, the number of viewports popped
# gets put on the display list
draw.details.default <- function(x, x.wrapped, recording) {
  pop.viewport(x, recording)
}

draw.details.glist <- function(x, x.wrapped, recording) {
}

draw.details.viewport <- function(x, x.wrapped, recording) {
  push.viewport(x, recording=FALSE)
}

print.grob <- function(x, ...) {
  cl <- class(get.value.grob(x))
  print(paste(cl[1:(length(cl)-1)], collapse=" "))
}

# Make an explicit copy of a grob (i.e., not just another reference
# to the same grob)
grid.copy <- function(grob) {
  list.struct <- grid.get(grob)
  cl <- class(list.struct)
  cl <- cl[1:(length(cl)-1)]
  grid.grob(list.struct, cl, draw=FALSE)
}

