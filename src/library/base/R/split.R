#  File src/library/base/R/split.R
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

split <- function(x, f, drop = FALSE, ...) UseMethod("split")

split.default <- function(x, f, drop = FALSE, ...)
{
    if(length(list(...))) .NotYetUsed(deparse(...), error = FALSE)

    if (is.list(f)) f <- interaction(f, drop = drop)
    else if (drop || !is.factor(f)) # drop extraneous levels
	f <- factor(f)
    storage.mode(f) <- "integer"  # some factors have double
    if (is.null(attr(x, "class")))
	return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    y <- vector("list", length(lf))
    names(y) <- lf
    ind <- .Internal(split(seq_along(f), f))
    for(k in lf) y[[k]] <- x[ind[[k]]]
    y
}

## This is documented to work for matrices too
split.data.frame <- function(x, f, drop = FALSE, ...)
    lapply(split(x = seq_len(nrow(x)), f = f, drop = drop, ...),
           function(ind) x[ind, , drop = FALSE])

## split.data.frame <- function(x, f, drop = FALSE, ...)
## {
##     inds <- split(seq_len(nrow(x)), f, drop = drop, ...)
##     rn <- row.names(x)
##     cl <- class(x)
##     class(x) <- NULL
##     a <- attributes(x)
##     a <- a[names(a) != "row.names"]
##     lapply(inds, function(i) {
##         z <- lapply(x, "[", i)
##         if(length(a)) attributes(z) <- a
##         class(z) <- cl
##         attr(z, "row.names") <- rn[i]
##         z
##     })
## }


`split<-` <- function(x, f, drop = FALSE, ..., value) UseMethod("split<-")

`split<-.default` <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_along(x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i] <- value[[j]]
    }
    x
}

## This is documented to work for matrices too
`split<-.data.frame` <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq_len(nrow(x)), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i,] <- value[[j]]
    }
    x
}

unsplit <-function (value, f, drop = FALSE)
{
    len <- length(if (is.list(f)) f[[1L]] else f)
    if (is.data.frame(value[[1L]])) {
        x <- value[[1L]][rep(NA, len),, drop = FALSE]
        rownames(x) <- unsplit(lapply(value, rownames), f, drop = drop)
    } else
        x <- value[[1L]][rep(NA, len)]
    split(x, f, drop = drop) <- value
    x
}
