#  File src/library/stats/R/lowess.R
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

lowess <- function(x, y=NULL, f=2/3, iter=3, delta=.01*diff(range(xy$x[o]))) {
    xy <- xy.coords(x,y)
    n <- as.integer(length(xy$x))
    if (is.na(n)) stop("invalid length(x)")
    if(n == 0L) stop("'x' is empty")
    o <- order(xy$x)
    .C(C_lowess,
       x = as.double(xy$x[o]),
       as.double(xy$y[o]),
       n,
       as.double(f),
       as.integer(iter),
       as.double(delta),
       y = double(n),
       double(n),
       double(n), PACKAGE = "stats")[c("x","y")]
}
