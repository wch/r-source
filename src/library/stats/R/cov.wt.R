#  File src/library/stats/R/cov.wt.R
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

cov.wt <- function(x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE,
                   method = c("unbiased", "ML"))
{
    if (is.data.frame(x))
	x <- as.matrix(x)
    else if (!is.matrix(x))
	stop("'x' must be a matrix or a data frame")
    if (!all(is.finite(x)))
	stop("'x' must contain finite values only")
    n <- nrow(x)
    if (with.wt <- !missing(wt)) {
	if (length(wt) != n)
	    stop("length of 'wt' must equal the number of rows in 'x'")
	if (any(wt < 0) || (s <- sum(wt)) == 0)
	    stop("weights must be non-negative and not all zero")
	wt <- wt / s
    }
    if (is.logical(center)) {
	center <- if (center) colSums(wt * x) else 0
    } else {
	if (length(center) != ncol(x))
	    stop("length of 'center' must equal the number of columns in 'x'")
    }
    x <- sqrt(wt) * sweep(x, 2, center, check.margin=FALSE)
    cov <-
        switch(match.arg(method),
               "unbiased" = crossprod(x) / (1 - sum(wt^2)),
               "ML" = crossprod(x))
    y <- list(cov = cov, center = center, n.obs = n)
    if (with.wt) y$wt <- wt
    if (cor) { ## as cov2cor():
        Is <- 1 / sqrt(diag(cov))
        R <- cov
        R[] <- Is * cov * rep(Is, each = nrow(cov))
	y$cor <- R
    }
    y
}
