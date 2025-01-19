#  File src/library/base/R/sets.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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
## no items from x or y retains the class, and the dimension has at most
## length one.
## </NOTE>

.set_ops_need_as_vector <- function(x, y) {
    ((length(dim(x)) >= 2L) ||
     (length(dim(y)) >= 2L) ||
     (!isa(x, class(y)) && !isa(y, class(x))) ||
     !identical(class(tryCatch(x[0L], error = identity)), class(x)) ||
     !identical(class(tryCatch(y[0L], error = identity)), class(y)))
}

## <NOTE>
## We tend to not add internal utility functions to base as these cannot
## really be hidden.  Otherwise, we could do something like
##    .is_vector_like <- function(x) {
##         (length(dim(x)) < 2L) &&
##          identical(class(tryCatch(x[0L], error = identity)),
##                    class(x))
##     }
## (or perhaps even add a .is_vector_like() generic with the above as
## default method, and
##    .is_same_kind <- function(x, y) {
##        isa(x, class(y)) || isa(y, class(x)
## }
## and finally
##     .set_ops_need_as_vector <- function(x, y) {
##          !.is_vector_like(x) ||
##          !.is_vector_like(y) ||
##          !.is_same_kind(x, y)
## }
## instead of putting everything into one function.
## </NOTE>

union <-
function(x, y)
{
    if(.set_ops_need_as_vector(x, y)) {
        x <- as.vector(x)
        y <- as.vector(y)
    } else if(!isa(x, class(y)))
        x <- c(y[0L], x)
    x <- unique(x)
    names(x) <- NULL
    y <- unique(y)
    names(y) <- NULL
    c(x, y[match(y, x, 0L) == 0L])
}

intersect <-
function(x, y)
{
    if(is.null(x) || is.null(y)) return(NULL)
    if(.set_ops_need_as_vector(x, y)) {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    x <- unique(x)
    names(x) <- NULL
    y <- unique(y)
    names(y) <- NULL
    ## Combining with y[0L] in the common class case is needed e.g. for 
    ## factors to combine levels, and otherwise to get a common mode.
    c(x[match(x, y, 0L) > 0L], y[0L])
}

setdiff <-
function(x, y)
{
    if(is.null(x)) return(NULL)
    if(.set_ops_need_as_vector(x, y)) {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    x <- unique(x)
    names(x) <- NULL
    y <- unique(y)
    names(y) <- NULL
    x[match(x, y, 0L) == 0L]
}

setequal <-
function(x, y)
{
    if(.set_ops_need_as_vector(x, y)) {
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
    if(.set_ops_need_as_vector(x, y)) {
        x <- as.vector(x)
        y <- as.vector(y)
    }
    match(x, y, 0L) > 0L
}
