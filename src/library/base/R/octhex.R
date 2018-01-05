#  File src/library/base/R/octhex.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

format.octmode <- function(x, width = NULL, ...)
{
    isna <- is.na(x)
    y <- as.integer(x[!isna])
    fmt <- if(!is.null(width)) paste0("%0", width, "o") else "%o"
    ans <- rep.int(NA_character_, length(x))
    ans0 <- sprintf(fmt, y)
    if(is.null(width) && length(y) > 1L) {
        ## previous version padded with zeroes to a common field width
        nc <- max(nchar(ans0))
        ans0 <- sprintf(paste0("%0", nc, "o"), y)
    }
    ans[!isna] <- ans0
    dim(ans) <- dim(x)
    dimnames(ans) <- dimnames(x)
    names(ans) <- names(x)
    ans
}

as.character.octmode <- function(x, ...) format.octmode(x, ...)

print.octmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

`[.octmode` <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

as.octmode <- function(x)
{
    if(inherits(x, "octmode")) return(x)
    if(is.double(x) && all(is.na(x) | x == as.integer(x))) x <- as.integer(x)
    if(is.integer(x)) return(structure(x, class="octmode"))
    if(is.character(x)) {
        z <- strtoi(x, 8L)
        if(!any(is.na(z) | z < 0)) return(structure(z, class="octmode"))
    }
    stop("'x' cannot be coerced to class \"octmode\"")
}

## BioC packages cellHTS2 and flowCore misuse this for doubles,
## hence the as.integer() call
format.hexmode <- function(x, width = NULL, upper.case = FALSE, ...)
{
    isna <- is.na(x)
    y <- as.integer(x[!isna])
    fmt0 <- if(upper.case) "X" else "x"
    fmt <- if(!is.null(width)) paste0("%0", width, fmt0) else paste0("%", fmt0)
    ans <- rep.int(NA_character_, length(x))
    ans0 <- sprintf(fmt, y)
    if(is.null(width) && length(y) > 1L) {
        ## previous version padded with zeroes to a common field width
        nc <- max(nchar(ans0))
        ans0 <- sprintf(paste0("%0", nc, fmt0), y)
    }
    ans[!isna] <- ans0
    dim(ans) <- dim(x)
    dimnames(ans) <- dimnames(x)
    names(ans) <- names(x)
    ans
}

as.character.hexmode <- function(x, ...) format.hexmode(x, ...)

print.hexmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

`[.hexmode` <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

as.hexmode <- function(x)
{
    if(inherits(x, "hexmode")) return(x)
    if(is.double(x) && all(is.na(x) | x == as.integer(x))) x <- as.integer(x)
    if(is.integer(x)) return(structure(x, class = "hexmode"))
    if(is.character(x)) {
        z <- strtoi(x, 16L)
        if(!any(is.na(z) | z < 0)) return(structure(z, class = "hexmode"))
    }
    stop("'x' cannot be coerced to class \"hexmode\"")
}


`!.octmode` <- function(a) as.octmode(bitwNot(as.octmode(a)))

`&.octmode` <- function(a, b) as.octmode(bitwAnd(as.octmode(a), as.octmode(b)))
`|.octmode` <- function(a, b) as.octmode(bitwOr(as.octmode(a), as.octmode(b)))
## FIXME: xor() is not generic (yet?).
## xor.octmode <- function(a, b) as.octmode(bitwXor(as.octmode(a), as.octmode(b)))

`!.hexmode` <- function(a) as.hexmode(bitwNot(as.hexmode(a)))

`&.hexmode` <- function(a, b) as.hexmode(bitwAnd(as.hexmode(a), as.hexmode(b)))
`|.hexmode` <- function(a, b) as.hexmode(bitwOr(as.hexmode(a), as.hexmode(b)))
## FIXME: xor() is not generic (yet?).
## xor.hexmode <- function(a, b) as.hexmode(bitwXor(as.hexmode(a), as.hexmode(b)))
