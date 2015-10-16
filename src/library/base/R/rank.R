#  File src/library/base/R/rank.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

rank <- function(x, na.last = TRUE,
		 ties.method = c("average", "first", "last", "random", "max", "min"))
{
    nas <- is.na(x)
    nm <- names(x)
    ties.method <- match.arg(ties.method)
    ## To preserve past behaviour
    if(is.factor(x)) x <- as.integer(x)
    x <- x[!nas]
    ## we pass length(x) to allow
    y <- switch(ties.method,
		"average" = , "min" = , "max" =
		.Internal(rank(x, length(x), ties.method)),
		"first" = sort.list(sort.list(x)),
                "last"  = sort.list(sort.list(x, decreasing=TRUE),
                                    decreasing=TRUE),
		"random" = sort.list(order(x, stats::runif(sum(!nas)))))
    ## the internal code has ranks in [1, length(y)]
    if(!is.na(na.last) && any(nas)) {
	yy <- NA
	NAkeep <- (na.last == "keep")
	if(NAkeep || na.last) {
	    yy[!nas] <- y
	    if(!NAkeep) yy[nas] <- (length(y) + 1L) : length(yy)
	} else {
	    len <- sum(nas)
	    yy[!nas] <- y + len
	    yy[nas] <- seq_len(len)
	}
	y <- yy
	names(y) <- nm
    } else names(y) <- nm[!nas]
    y
}
