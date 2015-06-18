#  File src/library/utils/R/combn.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

combn <- function(x, m, FUN = NULL, simplify = TRUE, ...)
{
    ## DATE WRITTEN: 14 April 1994	    LAST REVISED:  10 July 1995
    ## AUTHOR:	Scott Chasalow
    ##
    ## DESCRIPTION:
    ##	Generate all combinations of the elements of x taken m at a time.
    ##	If x is a positive integer,  returns all combinations
    ##	of the elements of seq(x) taken m at a time.
    ##	If argument "FUN" is not null,	applies a function given
    ##	by the argument to each point.	If simplify is FALSE,  returns
    ##	a list; else returns a vector or an array.  "..." are passed
    ##	unchanged to function given by argument FUN,  if any.

    stopifnot(length(m) == 1L, is.numeric(m))
    if(m < 0) stop("m < 0", domain = NA)
    if(is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == x)
	x <- seq_len(x)
    n <- length(x)
    if(n < m) stop("n < m", domain = NA)
    x0 <- x
    if(simplify) {
        if(is.factor(x)) x <- as.integer(x)
    }
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- seq_len(m)
    nofun <- is.null(FUN)
    if(!nofun && !is.function(FUN))
	stop("'FUN' must be a function or NULL")
    # first result : what kind, what length,.. ?
    len.r <- length(r <- if(nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m))) # >= 1
    if(simplify) {
	dim.use <-
	    if(nofun)
		c(m, count) # matrix also when count = 1
	    else {
		d <- dim(r)
		if(length(d) > 1L)
		    c(d, count)
		else if(len.r > 1L)
		    c(len.r, count)
		else # MM: *still* a matrix - a la "drop = FALSE"
		    c(d, count)
	    } ## NULL in all 'else' cases
    }

    if(simplify)
        out <- matrix(r, nrow = len.r, ncol = count) # matrix for now
    else {
	out <- vector("list", count)
	out[[1L]] <- r
    }

    if(m > 0) {
	i <- 2L
	nmmp1 <- n - m + 1L	 # using 1L to keep integer arithmetic
	while(a[1L] != nmmp1) {
	    if(e < n - h) {
		h <- 1L
		e <- a[m]
		j <- 1L
	    }
	    else {
		e <- a[m - h]
		h <- h + 1L
		j <- 1L:h
	    }
	    a[m - h + j] <- e + j
	    r <- if(nofun) x[a] else FUN(x[a], ...)
	    if(simplify) out[, i] <- r else out[[i]] <- r
	    i <- i + 1L
	}
    }
    if(simplify) {
        if(is.factor(x0)) {
            levels(out) <- levels(x0)
            class(out) <- class(x0)
        }
        dim(out) <- dim.use
    }
    out
}
