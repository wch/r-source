#  File src/library/grid/R/just.R
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

# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/lattice.h
# NOTE: the result of match() is an integer, but subtracting 1 converts
# to real => have to convert back to integer for passing to C code

# If the user specifies two values, the first is horizontal
# justification and the second is vertical

# If the user specifies only one value, use the following
# conversion table to give a second default value
#
# bottom  -->  centre, bottom
# left    -->  left,   centre
# right   -->  right,  centre
# top     -->  centre, top
# centre  -->  centre, centre

valid.charjust <- function(just) {
  if (length(just) == 1) {
    # single value may be any valid just
    just <- as.integer(match(just[1L], c("left", "right", "bottom", "top",
                                        "centre", "center")) - 1)
    if (anyNA(just))
      stop("invalid justification")
  } else if (length(just) > 1) {
    # first value must be one of "left", "right", "centre", or "center"
    just[1L] <- as.integer(match(just[1L], c("left", "right", "bottom", "top",
                                           "centre", "center")) - 1)
    if (!(just[1L] %in% c(0, 1, 4, 5)))
      stop("invalid horizontal justification")
    # second value must be one of "bottom", "top", "centre", or "center"
    just[2L] <- as.integer(match(just[2L], c("left", "right", "bottom", "top",
                                           "centre", "center")) - 1)
    if (!(just[2L] %in% c(2, 3, 4, 5)))
      stop("invalid vertical justification")
    just <- as.integer(just)
  }
  # Extend to length 2 if necessary
  if (length(just) < 2) {
    if (length(just) == 0)
      just <- c(4, 4)
    else
      just <- switch (just[1L] + 1,
                      c(0, 4), # left
                      c(1, 4), # right
                      c(4, 2), # bottom
                      c(4, 3), # top
                      c(4, 4), # centre
                      c(4, 4)) # center
  }
  # Convert to numeric
  just <- c(switch(just[1L] + 1, 0, 1, NA, NA, 0.5, 0.5),
            switch(just[2L] + 1, NA, NA, 0, 1, 0.5, 0.5))
  # Final paranoid check
  if (anyNA(just))
    stop("invalid justification")
  just
}

valid.numjust <- function(just) {
  if (length(just) == 0) {
    c(0.5, 0.5)
  } else {
    if (length(just) < 2) {
      c(just, 0.5)
    } else {
      just
    }
  }
}

valid.just <- function(just) {
  if (is.character(just))
    valid.charjust(just)
  else {
    valid.numjust(as.numeric(just))
  }
}

resolveHJust <- function(just, hjust) {
  if (is.null(hjust) || length(hjust) == 0)
    valid.just(just)[1L]
  else
    hjust
}

resolveVJust <- function(just, vjust) {
  if (is.null(vjust) || length(vjust) == 0)
    valid.just(just)[2L]
  else
    vjust
}
