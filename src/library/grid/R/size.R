# These functions are used to evaluate "grobwidth" and
# "grobheight" units.
# They are usually called from within the C code
# (specifically, from within unit.c)
# It should be noted that they only give the width/height
# of the grob in the current drawing context
# (i.e., evaluating the width/height in another context
#  will not necessarily give the same result)

# The C code to evaluate "grobwidth" and "grobheight" calls
# the preDrawDetails and postDrawDetails generics before and
# after the call to width/height() to allow for complex grobs which
# construct their own viewports.

#########
# WIDTHS
#########

# We are doing this in R code to provide generics like widthDetails
# so that users can customise the behaviour for complex grobs by
# writing their own (R code!) methods
width <- function(x) {
  widthDetails(x)
}

widthDetails <- function(x) {
  UseMethod("widthDetails", x)
}

widthDetails.default <- function(x) {
  unit(1, "null")
}

#########
# HEIGHTS
#########
height <- function(x) {
  heightDetails(x)
}

heightDetails <- function(x) {
  UseMethod("heightDetails", x)
}

heightDetails.default <- function(x) {
  unit(1, "null")
}

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
# watch out for that;  the second sort I just don't want to have to deal with.
#
# On the other hand, dimensions which do not depend on the parent context
# are much easier to deal with (e.g., "inches", "cm", "lines", ...)
#
# So this function takes a unit and returns absolute values
# untouched and replaces other values with unit(1, "null")

absolute.size <- function(unit) {
  absolute.units(unit)
}

