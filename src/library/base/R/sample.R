#  File src/library/base/R/sample.R
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

sample <- function(x, size, replace=FALSE, prob=NULL)
{
    if(length(x) == 1L && is.numeric(x) && x >= 1) {
	if(missing(size)) size <- x
	.Internal(sample(x, size, replace, prob))
    }
    else {
	if(missing(size)) size <- length(x)
	x[.Internal(sample(length(x), size, replace, prob))]
    }
}

sample.int  <- function(n, size=n, replace=FALSE, prob=NULL)
    .Internal(sample(n, size, replace, prob))
