#  File src/library/base/R/rank.R
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

rank <- function(x, na.last = TRUE,
		 ties.method = c("average", "first", "random", "max", "min"))
{
    nas <- is.na(x)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    ## To preserve past behaviour
    if(is.factor(x)) x <- as.integer(x)
    xx <- x[!nas]
    ## we pass length(xx) to allow
    y <- switch(ties.method,
		"average" = , "min" = , "max" =
		.Internal(rank(xx, length(xx), ties.method)),
		"first" = sort.list(sort.list(xx)),
		"random" = sort.list(order(xx, stats::runif(sum(!nas)))))
    if(!is.na(na.last) && any(nas)) {
	## the internal code has ranks in [1, length(y)]
	yy <- integer(length(x))
	storage.mode(yy) <- storage.mode(y) # integer or double
	yy <- NA
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    yy[!nas] <- y
	    if(!NAkeep) yy[nas] <- (length(y) + 1L) : length(yy)
	} else {
	    len <- sum(nas)
	    yy[!nas] <- y + len
	    yy[nas] <- 1L : len
	}
	y <- yy
	names(y) <- nm
    } else names(y) <- nm[!nas]
    y
}
