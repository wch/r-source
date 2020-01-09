#  File src/library/utils/R/head.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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


## check for acceptable n, called by several head() and tail() methods
checkHT <- function(n, d) {
    len <- length(n)
    msg <- if(len == 0 || all(is.na(n)))
        gettext("invalid 'n' -  must contain at least one non-missing element, got none.")
    else if(is.null(d) && len > 1L)
        gettextf("invalid 'n' - must have length one when dim(x) is NULL, got %d", len)
    else if(!is.null(d) && len > length(d))
        gettextf("invalid 'n' - length(n) must be <= length(dim(x)), got %d > %d",
                 len, length(d))
    else return(invisible())
    stop(msg, domain = NA)
}


head <- function(x, ...) UseMethod("head")

head.default <- function(x, n = 6L, ...)
{
    checkHT(n, dx <- dim(x))
    if(!is.null(dx))
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
    checkHT(n, d <- dim(x))
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
    checkHT(n, dim(x))
    r <- format(x)
    dimnames(r) <- list(rep.int("", nrow(r)),
                        rep.int("", ncol(r)))
    noquote(head.matrix(r, n = n + nrow(r) - nrow(x), ...))
}

head.function <- function(x, n = 6L, ...)
{
    checkHT(n, dim(x))
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(head(lines, n=n))
}

tail <- function(x, ...) UseMethod("tail")

tail.default <- function (x, n = 6L, keepnums = FALSE, addrownums, ...)
{
    checkHT(n, dx <- dim(x))
    if(!is.null(dx))
        tail.array(x, n=n, keepnums=keepnums, addrownums=addrownums, ...)
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
tail.array <- function(x, n = 6L, keepnums = TRUE, addrownums, ...)
{
    if(!missing(addrownums)) {
        .Deprecated(msg = gettext("tail(., addrownums = V) is deprecated.\nUse ",
                                  "tail(., keepnums = V) instead.\n"))
        if(missing(keepnums))
            keepnums <- addrownums
    }

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
    if (keepnums && length(d) > 1L) {
        jj <- if(!is.null(adnms <- dimnames(ans)[ii]))
                  which(vapply(adnms, is.null, NA)) else ii
        ## For data.frames dimnames(.) never has null elements
        ## but dimnames(.)[numeric()]<-list() converts default
        ## row.names from INTSXP to AltString STRSXP, so avoid it.
        if(length(jj) > 0) {
            ## jj are indices in dim(x)
            ## k are indices in jj/sel
            dimnames(ans)[jj] <- lapply(seq_along(jj),
                                        function(k) {
                ## 1 is rownames, pseudo-col so format [.,]
                ## 2 is colnames, pseudo-row so straight [,.]
                ## >2, return correct/orig indices
                if((dnum <- jj[k]) == 1L)
                    format(sprintf("[%d,]", sel[[k]]),
                           justify = "right")
                else if(dnum == 2L)
                    sprintf("[,%d]", sel[[k]])
                else ## dnum > 2
                    sel[[k]]
            })
        }
    }
    ans
}

## ../NAMESPACE defines  data.frame and table  method via tail.array, too :
## S3method(tail, data.frame, tail.array) ... and ditto for 'table'

tail.ftable <- function(x, n = 6L, keepnums = FALSE, addrownums, ...) {
    r <- format(x)
    dimnames(r) <- list(if(!keepnums) rep.int("", nrow(r)),
			if(!keepnums) rep.int("", ncol(r)))
    noquote(tail.matrix(r, n = n, keepnums = keepnums, addrownums = addrownums, ...))
}

tail.function <- function(x, n = 6L, ...)
{
    lines <- as.matrix(deparse(x))
    dimnames(lines) <- list(seq_along(lines),"")
    noquote(tail(lines, n=n))
}
