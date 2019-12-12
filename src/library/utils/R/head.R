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

head.default <- function(x, n = 6L, ...)
{
    if(!is.null(dx <- dim(x)))
        head.array(x, n, ...)
    else if(length(n) == 1L) {
        n <- if (n < 0L) max(length(x) + n, 0L) else min(n, length(x))
        x[seq_len(n)]
    } else
        stop(gettextf("no method found for %s(., n=%s) and class %s",
                      "head", deparse(n), sQuote(class(x))),
             domain = NA)
}

## head.matrix and tail.matrix are now exported (to be used for other classes)
head.matrix <-
## used on arrays (incl. matrices), data frames, .. :
head.array <- function(x, n = 6L, ...)
{
    d <- dim(x)
    args <- rep(alist(x, , drop = FALSE), c(1L, length(d), 1L))
    ## non-specified dimensions (ie dims > length(n) or n[i] is NA) will stay missing / empty:
    ii <- which(!is.na(n[seq_along(d)]))
    args[1L + ii] <- lapply(ii, function(i)
        seq_len(if((ni <- n[i]) < 0L) max(d[i] + ni, 0L) else min(ni, d[i]) ))
    do.call("[", args)
}
## ../NAMESPACE defines  data.frame  method via head.array, too :
## S3method(head, data.frame, head.array)


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

tail.default <- function (x, n = 6L, addrownums = FALSE, ...)
{
    if(!is.null(dx <- dim(x)))
        tail.array(x, n=n, addrownums=addrownums, ...)
    else if(length(n) == 1L) {
        xlen <- length(x)
        n <- if (n < 0L) max(xlen + n, 0L) else min(n, xlen)
        x[seq.int(to = xlen, length.out = n)]
    } else
        stop(gettextf("no method found for %s(., n=%s) and class %s",
                      "tail", deparse(n), sQuote(class(x))),
             domain = NA)
}

## tail.matrix is exported (to be reused)
tail.matrix <-
tail.array <- function(x, n = 6L, addrownums = TRUE, ...)
{
    d <- dim(x)
    ## non-specified dimensions (ie dims > length(n) or n[i] is NA) will stay missing / empty:
    ii <- which(!is.na(n[seq_along(d)]))
    sel <- lapply(ii, function(i) {
        di <- d[i]
        ni <- n[i]
        seq.int(to = di, ## handle negative n's; result is *integer* iff ds[] is
                length.out = if(ni < 0L) max(di + ni, 0L) else min(ni, di))
        })
    args <- rep(alist(x, , drop = FALSE), c(1L, length(d), 1L))
    args[1L + ii] <- sel
    ans <- do.call("[", args)
    if (addrownums && length(d) > 1L && is.null(rownames(x)))
        ## first element of sel is the rows selected
	rownames(ans) <- format(sprintf("[%d,]", sel[[1]]), justify="right")
    ans
}
## ../NAMESPACE defines  data.frame and table  method via tail.array, too :
## S3method(tail, data.frame, tail.array) ... and ditto for 'table'

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
