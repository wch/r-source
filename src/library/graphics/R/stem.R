#  File src/library/graphics/R/stem.R
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

stem <- function(x, scale = 1, width = 80, atom = 0.00000001)
{
    if (!is.numeric(x) ) stop("'x' must be numeric")
    x <- x[is.finite(x)]
    n <- as.integer(length(x))
    if (is.na(n)) stop("invalid length(x)")
    if (n == 0) stop("no finite and non-missing values")
    if (scale <= 0) stop("'scale' must be positive") # unlike S
    .Call(C_StemLeaf, as.double(x), scale, width, atom)
    invisible(NULL)
}
