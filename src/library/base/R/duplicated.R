#  File src/library/base/R/duplicated.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

duplicated <- function(x, incomparables = FALSE, ...) UseMethod("duplicated")

duplicated.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)
    .Internal(duplicated(x, incomparables, fromLast,
                         if(is.factor(x)) min(length(x), nlevels(x) + 1L) else nmax))

duplicated.data.frame <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    if(length(x) != 1L)
        duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
    else duplicated(x[[1L]], fromLast = fromLast, ...)
}

duplicated.matrix <- duplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    res <- duplicated.default(temp, fromLast = fromLast, ...)
    dim(res) <- dim(temp)
    dimnames(res) <- dimnames(temp)
    res
}

anyDuplicated <- function(x, incomparables = FALSE, ...)
    UseMethod("anyDuplicated")

anyDuplicated.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
    .Internal(anyDuplicated(x, incomparables, fromLast))


anyDuplicated.data.frame <-
    function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    anyDuplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
}

anyDuplicated.matrix <- anyDuplicated.array <-
    function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    anyDuplicated.default(temp, fromLast = fromLast)
}

unique <- function(x, incomparables = FALSE, ...) UseMethod("unique")


## NB unique.default is used by factor to avoid unique.matrix,
## so it needs to handle some other cases.
unique.default <-
    function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...)
{
    if(is.factor(x)) {
        z <- .Internal(unique(x, incomparables, fromLast,
                              min(length(x), nlevels(x) + 1L)))
 	return(factor(z, levels = seq_len(nlevels(x)), labels = levels(x),
               ordered = is.ordered(x)))
    }
    z <- .Internal(unique(x, incomparables, fromLast, nmax))
    if(inherits(x, "POSIXct"))
        structure(z, class = class(x), tzone = attr(x, "tzone"))
    else if(inherits(x, "Date"))
        structure(z, class = class(x))
    else z
}

unique.data.frame <- function(x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    x[!duplicated(x, fromLast = fromLast, ...),  , drop = FALSE]
}

unique.matrix <- unique.array <-
    function(x, incomparables = FALSE , MARGIN = 1, fromLast = FALSE, ...)
{
    if(!identical(incomparables, FALSE))
	.NotYetUsed("incomparables != FALSE")
    dx <- dim(x)
    ndim <- length(dx)
    if (length(MARGIN) > ndim || any(MARGIN > ndim))
        stop(gettextf("MARGIN = %d is invalid for dim = %d", MARGIN, dx),
             domain = NA)
    collapse <- (ndim > 1L) && (prod(dx[-MARGIN]) > 1L)
    temp <- if(collapse) apply(x, MARGIN, function(x) paste(x, collapse = "\r")) else x
    args <- rep(alist(a=), ndim)
    names(args) <- NULL
    args[[MARGIN]] <- !duplicated.default(temp, fromLast = fromLast, ...)
    do.call("[", c(list(x), args, list(drop = FALSE)))
}
