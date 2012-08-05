#  File src/library/grid/R/size.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
# X locations on edge
#########

xDetails <- function(x, theta) {
  UseMethod("xDetails")
}

xDetails.default <- function(x, theta) {
  unit(0.5, "npc")
}

#########
# Y locations on edge
#########

yDetails <- function(x, theta) {
  UseMethod("yDetails")
}

yDetails.default <- function(x, theta) {
  unit(0.5, "npc")
}

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

ascentDetails <- function(x) {
    UseMethod("ascentDetails", x)
}

ascentDetails.default <- heightDetails.default

ascentDetails.grob <- function(x) {
    heightDetails(x)
}

descentDetails <- function(x) {
    UseMethod("descentDetails", x)
}

descentDetails.default <- function(x) {
    unit(0, "inches")
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

