#  File src/library/base/R/sample.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

sample <- function(x, size, replace = FALSE, prob = NULL)
{
    if(length(x) == 1L && is.finite(x) && x >= 1) {
	if(missing(size)) size <- x
	sample.int(x, size, replace, prob)
    } else {
	if(missing(size)) size <- length(x)
	x[sample.int(length(x), size, replace, prob)]
    }
}

sample.int  <- function(n, size = n, replace = FALSE, prob = NULL,
                        useHash = (!replace && is.null(prob) && size <= n/2 && n > 1e7))
{
    if (useHash)
        .Internal(sample2(n, size))
    else .Internal(sample(n, size, replace, prob))
}
