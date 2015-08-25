#  File src/library/stats/R/lowess.R
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

lowess <- function(x, y = NULL, f = 2/3, iter = 3L,
                   delta = 0.01 * diff(range(x)))
{
    xy <- xy.coords(x,y)
    o <- order(xy$x)
    x <- as.double(xy$x[o])
    list(x = x, y = .Call(C_lowess, x, as.double(xy$y[o]), f, iter, delta))
}
