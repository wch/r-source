#  File src/library/base/R/sets.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

## <NOTE>
## The set ops have always been documented to work for args that are
## "same-kind" (same mode in the unclassed case) and sequences of items,
## i.e., "vector-like".
## In the "same-kind" case we test for vector-like whether subscripting
## no items from x or y retains the class.
## Where needed, we also check whether duplicated() on x or y has the
## same length as x or y: for consistency this could always be done, at
## a possible loss of efficiency where not needed.
## </NOTE>

union <-
function(x, y)
{
    if(is.null(x)) return(y)
    if(is.null(y)) return(x)
    cx <- class(x)
    cy <- class(y)
    if((isa(x, cy) || isa(y, cx)) &&
       identical(class(y0 <- y[integer()]), cy) &&
       (length(dx <- duplicated(x)) == length(x)) &&
       (length(dy <- duplicated(y)) == length(y))) {
        if(!isa(x, cy))
            x <- c(y0, x)
        x <- x[!dx]
        y <- y[!dy]
    } else {
        x <- as.vector(x)
        x <- x[!duplicated(x)]
        y <- as.vector(y)
        y <- y[!duplicated(y)]
    }
    c(x, y[match(y, x, 0L) == 0L])
}

intersect <-
function(x, y)
{
    if(is.null(x) || is.null(y))
        return(NULL)
    cx <- class(x)
    cy <- class(y)    
    if((isa(x, cy) || isa(y, cx)) &&
       identical(class(y0 <- y[integer()]), cy) &&
       (length(dx <- duplicated(x)) == length(x))) {
        x <- x[!dx]
    } else {
        x <- as.vector(x)
        x <- x[!duplicated(x)]
        y <- as.vector(y)
        y0 <- y[integer()]
    }
    ## Combining with y0 in the common class case is needed e.g. for
    ## factors to combine levels, and otherwise to get a common mode.
    c(x[match(x, y, 0L) > 0L], y0)
}

setdiff <-
function(x, y)
{
    if(is.null(x) || is.null(y))
        return(x)
    cx <- class(x)
    cy <- class(y)    
    if((isa(x, cy) || isa(y, cx)) &&
       identical(class(x[integer()]), cx) &&
       (length(dx <- duplicated(x)) == length(x))) {
        x <- x[!dx]
    } else {
        x <- as.vector(x)
        x <- x[!duplicated(x)]
        y <- as.vector(y)
    }
    x[match(x, y, 0L) == 0L]
}

setequal <-
function(x, y)
{
    cx <- class(x)
    cy <- class(y)    
    if(!((isa(x, cy) || isa(y, cx)) &&
         identical(class(x[integer()]), cx))) {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    !( anyNA(match(x, y)) || anyNA(match(y, x)) )
}

## same as %in% ( ./match.R ) but different arg names (and possible
## as.vactor() transformation in the case args are not vector-like
## same-kind. 
is.element <-
function(el, set)
{
    x <- el
    y <- set
    cx <- class(x)
    cy <- class(y)    
    if(!((isa(x, cy) || isa(y, cx)) &&
         identical(class(x[integer()]), cx))) {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    match(x, y, 0L) > 0L
}
