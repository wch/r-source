#  File src/library/base/R/sets.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

union <- function(x, y) {
    u <- as.vector(x)
    v <- as.vector(y)
    if(!is.object(x) || !is.object(y) ||
       !identical(class(x), class(y))) {
        x <- u
        y <- v
    }
    z <- c(x[!duplicated(u)],
           y[!duplicated(v) & (match(v, u, 0L) == 0L)])
    names(z) <- NULL
    z
}

intersect <- function(x, y)
{
    if(is.null(x) || is.null(y))
        return(NULL)
    u <- as.vector(x)
    v <- as.vector(y)
    if(!is.object(x) || !is.object(y) ||
       !identical(class(x), class(y))) {
        x <- u
        y <- v
    }
    z <- c(x[!duplicated(u) & (match(u, v, 0L) > 0L)],
           y[numeric()])
    ## (Could avoid combining with y[numeric()] in the common class
    ## case.)
    names(z) <- NULL
    z
}

setdiff <- function(x, y)
{
    if(!length(x))
        return(x)
    u <- as.vector(x)
    v <- as.vector(y)
    z <- x[!duplicated(u) & (match(u, v, 0L) == 0L)]
    names(z) <- NULL
    z
}

## speed optimization etc: R-devel, Jan.4-6, 2000; then again 15 yrs later
setequal <- function(x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    !( anyNA(match(x, y)) || anyNA(match(y, x)) )
}

## same as %in% ( ./match.R ) but different arg names, and use match()
## on as.vector() transformations for consistency with the other set
## functions.
is.element <- function(el, set)
    match(as.vector(el), as.vector(set), 0L) > 0L

