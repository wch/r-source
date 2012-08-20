#  File src/library/base/R/backsolve.R
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

forwardsolve <-
    function(l, x, k = ncol(l), upper.tri = FALSE, transpose = FALSE)
{
    l <- as.matrix(l)
    x.mat <- is.matrix(x)
    if(!x.mat) x <- as.matrix(x)
    z <- .Internal(backsolve(l, x, k, upper.tri, transpose))
    if(x.mat) z else drop(z)
}

backsolve <- function(r, x, k  = ncol(r), upper.tri = TRUE, transpose = FALSE)
{
    r <- as.matrix(r) # so ncol(r) works
    x.mat <- is.matrix(x)
    if(!x.mat) x <- as.matrix(x)
    z <- .Internal(backsolve(r, x, k, upper.tri, transpose))
    if(x.mat) z else drop(z)
}
