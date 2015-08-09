#  File src/library/grid/R/interactive.R
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


grid.locator <- function(unit="native") {
  location <- c(grid.Call(L_locator), 1)
  if (is.na(location[1L]))
    invisible(NULL)
  else {
    transform <- solve(current.transform())
    location <- (location %*% transform)
    # The inverse viewport transform is from device coordinates into
    # inches relative to the current viewport
    location <- unit(location/location[3L], "inches")
    list(x=convertX(location[1L], unit),
         y=convertY(location[2L], unit))
  }
}

