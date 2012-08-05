#  File src/library/utils/R/head.R
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

### placed in the public domain 2002
### Patrick Burns patrick@burns-stat.com
###
### Adapted for negative arguments by Vincent Goulet
### <vincent.goulet@act.ulaval.ca>, 2006

head <- function(x, ...) UseMethod("head")

head.default <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
    x[seq_len(n)]
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.data.frame <- head.matrix <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
    x[seq_len(n), , drop=FALSE]
}
head.table  <- function(x, n = 6L, ...) {
    (if(length(dim(x)) == 2L) head.matrix else head.default)(x, n=n)
}

head.ftable <- function(x, n = 6L, ...) {
    r <- format(x)
    dimnames(r) <- list(rep.int("", nrow(r)), rep.int("", ncol(r)))
    noquote(head.matrix(r, n = n + nrow(r) - nrow(x), ...))
}

head.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    xlen <- length(x)
    n <- if (n < 0L) max(xlen + n, 0L) else min(n, xlen)
    x[seq.int(to = xlen, length.out = n)]
}

tail.data.frame <- function(x, n = 6L, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    x[seq.int(to = nrx, length.out = n), , drop = FALSE]
}

tail.matrix <- function(x, n = 6L, addrownums = TRUE, ...)
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    sel <- seq.int(to = nrx, length.out = n)
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x)))
    	rownames(ans) <- paste0("[", sel, ",]")
    ans
}
tail.table  <- function(x, n = 6L, addrownums = TRUE, ...) {
    (if(length(dim(x)) == 2L) tail.matrix else tail.default)(x, n=n,
	      addrownums = addrownums, ...)
}

tail.ftable <- function(x, n = 6L, addrownums = FALSE, ...) {
    r <- format(x)
    dimnames(r) <- list(if(!addrownums) rep.int("", nrow(r)),
			rep.int("", ncol(r)))
    noquote(tail.matrix(r, n = n, addrownums = addrownums, ...))
}

tail.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(tail(lines, n=n))
}
