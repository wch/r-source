# These functions are used to evaluate "grobwidth" and
# "grobheight" units.
# They are actually called from within the C code
# (specifically, from within unit.c) and should NOT be called
# from the command line in normal use.
# The width.pre function sets up the correct graphical context (
# gpar settings) for the grob.  The basic idea is that the width
# of a grob has to be evaluated within the same context as would
# be used to draw the grob.  For simple grobs, there should be
# nothing to do beyond the default given here.

# NOTE that we do NOT push any viewports.  That would probably create
# an infinite loop (because push.viewport would call set.viewport
# which would attempt to recalculate the entire viewport transform,
# which may get back to here if we originally got here due to
# calculating a viewport transform;  i.e., if we started with a
# viewport or layout that was using "grobwidth" or "grobheight" units)

# NOTE that the above note implies that we should NOT return a unit
# in the width.details method that relies on having the correct
# viewport set up.  In other words we should only return "absolute"
# units;  there is a function at the end of this file to help with this.

# For complex grobs, e.g., ones which
# construct their own viewports, it may be necessary to do extra
# setting up by writing a width.pre.details method.
# The width function just returns a unit object.
# The width.post function is important for reversing all of the
# setting up that was done in the width.pre function.  Again, for
# simple grobs there should be nothing to do beyond the default.

#########
# WIDTHS
#########

# NOTE that I have to do this in R rather than C code because
# I can't set par() values from C (yet !)
# ALSO NOTE that I have to set par() values for "strwidth" and 
# "strheight" units to work;  they rely on GStrWidth/Height which
# refer to par() values
# We are just setting graphical parameters
# We do NOT push any viewports !!
width.pre <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the width of the viewport rather than width of the grob
  if (!is.null(list.struct$vp))
    gp <- list.struct$vp$gp
  else
    gp <- list.struct$gp
  if (!is.null(gp))
    set.gpar(gp)
  if (!is.null(list.struct$vp))
    width.pre.details(list.struct$vp)
  else
    width.pre.details(list.struct)
}

width.pre.details <- function(x) {
  UseMethod("width.pre.details")
}

width.pre.details.default <- function(x) {}

width <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the width of the viewport rather than width of the grob
  if (!is.null(list.struct$vp))
    width.details(list.struct$vp)
  else
    width.details(list.struct)
}

width.details <- function(x) {
  UseMethod("width.details", x)
}

width.details.default <- function(x) {
  unit(1, "null")
}

# We are just unsetting graphical parameters
# We do NOT pop any viewports !!
width.post <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the width of the viewport rather than width of the grob
  if (!is.null(list.struct$vp))
    width.post.details(list.struct$vp)
  else
    width.post.details(list.struct)
  if (!is.null(list.struct$vp))
    gp <- list.struct$vp$gp
  else
    gp <- list.struct$gp
  if (!is.null(gp))
    unset.gpar(gp)
}

width.post.details <- function(x) {
  UseMethod("width.post.details")
}

width.post.details.default <- function(x) {}

#########
# HEIGHTS
#########

# We are just setting graphical parameters
# We do NOT push any viewports !!
height.pre <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the height of the viewport rather than height of the grob
  if (!is.null(list.struct$vp))
    gp <- list.struct$vp$gp
  else
    gp <- list.struct$gp
  if (!is.null(gp))
    set.gpar(gp)
  if (!is.null(list.struct$vp))
    height.pre.details(list.struct$vp)
  else
    height.pre.details(list.struct)
}

height.pre.details <- function(x) {
  UseMethod("height.pre.details")
}

height.pre.details.default <- function(x) {}

height <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the height of the viewport rather than height of the grob
  if (!is.null(list.struct$vp))
    height.details(list.struct$vp)
  else
    height.details(list.struct)
}

height.details <- function(x) {
  UseMethod("height.details", x)
}

height.details.default <- function(x) {
  unit(1, "null")
}

# We are just unsetting graphical parameters
# We do NOT pop any viewports !!
height.post <- function(x) {
  list.struct <- get.value(x)
  # If the grob has a viewport, then we treat it as if we want
  # the height of the viewport rather than height of the grob
  if (!is.null(list.struct$vp))
    height.post.details(list.struct$vp)
  else
    height.post.details(list.struct)
  if (!is.null(list.struct$vp))
    gp <- list.struct$vp$gp
  else
    gp <- list.struct$gp
  if (!is.null(gp))
    unset.gpar(gp)
}

height.post.details <- function(x) {
  UseMethod("height.post.details")
}

height.post.details.default <- function(x) {}

#########
# Some functions that might be useful for determining the sizes
# of your grobs
#########

# Dimensions which depend on the parent context EITHER don't make
# sense (e.g., no good to have the parent width depend on the child's
# width unit(1, "grobwidth", <child>), which depends on the parent's
# width unit(.1, "npc"), ...) OR are slightly ambiguous
# (e.g., gf <- grid.frame(); grid.pack(gf, grid.rect(width=unit(.1, "npc")))
# makes the area allocated to the rectangle .1 of the frame area, but
# then the rectangle only occupies .1 of _that_ allocated area;  my head
# hurts !).  The first sort will actually lead to infinite loops so
# I outlaw them;  the second sort I just don't want to have to deal with.
#
# On the other hand, dimensions which do not depend on the parent context
# are much easier to deal with (e.g., "inches", "cm", "lines", ...)
#
# So this function takes a unit and returns absolute values
# untouched and replaces other values with unit(1, "null")
#
# NOTE that I included "lines" amongst the absolute units above, even
# though these depend on the parent context in the sense that the
# parent may specify a value for lineheight or fontsize.
# This is ok because these are "absolute" graphical parameters that do not
# themselves depend on the parent's size (by contrast, "npc" units
# and "native" units depend on the parent's size).

absolute.size <- function(unit) {
  absolute.units(unit)
}

