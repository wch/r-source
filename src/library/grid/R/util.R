#  File src/library/grid/R/util.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/


# Define a convenience function that is easy to call from C code
grid.top.level.vp <- function() {
    vp <- viewport(clip=TRUE, name="ROOT")
    # Must mimic order of events in push.vp.viewport()
    # In particular, add 'gpar' and 'parentgpar' IN THE RIGHT ORDER
    # before calling pushedvp()
    vp$parentgpar <- gpar()
    vp$gpar <- gpar()
    pushedvp(vp)
}

# An accessor for getting at the grid global state structure
# to make debugging easier for me;  all I have to type is grid:::STATE()
STATE <- function() {
  get(".GRID.STATE", envir=.GridEvalEnv)
}

is.even <- function(x) x %% 2 == 0

is.odd <- function(x) !is.even(x)


grid.pretty <- function(range) {
  if (!is.numeric(range))
    stop("'range' must be numeric")
  .Call(L_pretty, range)
}

