#  File src/library/base/R/jitter.R
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

### Unimplemented Idea {for amount = NULL ?}
### Really "optimal" (e.g. for rug()), use a non-constant amount,
### e.g. use "d" = diff(xx)  BEFORE  taking min()...

jitter <- function(x, factor = 1, amount=NULL)
{
    if(length(x) == 0L)
	return(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    z <- diff(r <- range(x[is.finite(x)]))
    if(z == 0) z <- abs(r[1L])
    if(z == 0) z <- 1

    if(is.null(amount)) {		# default: Find 'necessary' amount
	d <- diff(xx <- unique(sort.int(round(x, 3 - floor(log10(z))))))
	d <- if(length(d)) min(d) else if(xx != 0) xx/10 else z/10
	amount <- factor/5 * abs(d)
    } else if(amount == 0)		# only then: S compatibility
	amount <- factor * (z/50)

    x + stats::runif(length(x),  - amount, amount)
}
