#  File src/library/utils/R/head.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

### placed in the public domain 2002
### Patrick Burns patrick@burns-stat.com
###
### Adapted for negative arguments by Vincent Goulet
### <vincent.goulet@act.ulaval.ca>, 2006
###
### Adapted for vector n in k-dimensional object
### case (df/matrix/array) by Gabriel Becker
### <gabembecker@gmail.com>, 2019

head <- function(x, ...) UseMethod("head")

## used on vectors, arrays, data frames, .. :
head.default <- function(x, n = 6L, ...)
{
    ## lapply over "dimensions" even for x without dim attribute.
    ds <- dim(x) %||% length(x)
    indvecs <- lapply(seq_along(ds), function(i) {
        ## All non-specified dimensions (ie dims > length(n) or n[i] is NA)
        ## are taken to be the maximum (display all indices in that dimension)
        di <- ds[i]
        ## NB: relies on n[i] returning NA if i > length(n)!
        ni <- if(!is.na(n[i])) n[i] else di
        ## negative indices work as a they always have, no restriction
        ## against mixing negative and positive ns across different dims
        ni <- if(ni < 0L) max(di + ni, 0L) else min(ni, di)
        seq_len(ni)
    })
    do.call(`[`, c(list(x), indvecs,
                   if(!is.null(dim(x))) list(drop = FALSE)))
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.matrix <- head.default
## head.array is not needed (nor would head.matrix be) as it would be identical

head.ftable <- function(x, n = 6L, ...) {
    r <- format(x)
    dimnames(r) <- list(rep.int("", nrow(r)),
                        rep.int("", ncol(r)))
    noquote(head.matrix(r, n = n + nrow(r) - nrow(x), ...))
}

head.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

## tail.array()  has  'addrownums = TRUE' ;  tail.default doesn't
.tailindices <- function(x, n)
{
    ds <- dim(x) %||% length(x)
    ## returns a list of vectors of indices in each dimension,
    ## regardless of length of the  n  argument :
    lapply(seq_along(ds), function(i) {
        dxi <- ds[i]
        ## select all indices (full dim) if not specified
        ni <- if(!is.na(n[i])) n[i] else dxi
        seq.int(to = dxi, ## handle negative n's; result is *integer* iff ds[] is
                length.out = if(ni < 0L) max(dxi + ni, 0L) else min(ni, dxi))
    })
}

# also for data.frame
tail.default <- function (x, n = 6L, ...)
{
    do.call(`[`, c(list(x), .tailindices(x, n),
                   if(!is.null(dim(x))) list(drop = FALSE)))
}


## tail.matrix is exported (to be reused)
tail.array <- tail.matrix <-
    tail.table <- function(x, n = 6L, addrownums = TRUE, ...)
{
    sel <- .tailindices(x, n)
    ans <- do.call(`[`, c(list(x), sel, drop = FALSE))

    if (length(dim(x)) > 1L && addrownums && is.null(rownames(x)))
        ## first element of sel is the rows selected
	rownames(ans) <- format(sprintf("[%d,]", sel[[1]]), justify="right")
    ans
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
