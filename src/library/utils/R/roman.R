#  File src/library/utils/R/roman.R
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

.as.roman <- function(x, check.range=TRUE)
{
    if(is.integer(x)) { }
    else if(is.double(x) || is.logical(x)) # <- as.roman(NA)
        x <- as.integer(x)
    else if(is.character(x)) {
	x <- if(all(dig.x <- !nzchar(x) | is.na(x) | grepl("^[[:digit:]]+$", x)))
		 as.integer(x)
	     else if(any(dig.x)) {
		 r <- suppressWarnings(as.integer(x))# NAs for all non-dig
		 r[!dig.x] <- .roman2numeric(x[!dig.x])
		 r
	     }
	     else ## no digits -- assume all roman characters
		 .roman2numeric(x)
    }
    else
        stop("cannot coerce 'x' to roman")
    if(check.range) x[x <= 0L | x >= 3900L] <- NA
    class(x) <- "roman"
    x
}
as.roman <- function(x) .as.roman(x, check.range=TRUE)

as.character.roman <- function(x, ...) .numeric2roman(x)

format.roman <- function(x, ...) format(as.character.roman(x), ...)

print.roman <- function(x, ...)
{
    print(noquote(as.character.roman(x)), ...)
    invisible(x)
}

`[.roman` <-
function(x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

Ops.roman <- function(e1, e2) {
    if(.Generic %in% c("+", "-", "*", "^", "%%", "%/%", "/")) { # "Arith" in S4 parlance:
	e1 <- .as.roman(e1, check.range=FALSE)
	e2 <- .as.roman(e2, check.range=FALSE)
	as.roman(NextMethod(.Generic))
    }
    else # "Compare" and "Logic" in S4 parlance; just work with integer:
	NextMethod(.Generic)
}

Summary.roman <- function(x, ..., na.rm=TRUE) {
    if(.Generic %in% c("any", "all"))
        NextMethod(.Generic)
    else # max, min, .. sum
        as.roman(NextMethod(.Generic))
}


## for recycling etc
rep.roman <- function(x, ...) structure(rep(unclass(x), ...), class = class(x))

## romans: used in both utility functions, and not unuseful in general:
.romans <-
    c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L, 5L,  4L, 1L)
names(.romans) <-
    c("M", "CM",  "D",  "CD", "C",  "XC", "L", "XL","X","IX","V","IV","I")
## Can *not* use stats {dependency cycle at build time} -- hence need our own:
## .setNames <- function (object = nm, nm) {
##     names(object) <- nm
##     object
## }
## .romans <- .setNames(
##     c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L, 5L,  4L, 1L),
##     c("M", "CM",  "D",  "CD", "C",  "XC", "L", "XL","X","IX","V","IV","I"))

.numeric2roman <-
function(x) {
    romaNs <- names(.romans)
    n2r <- function(z) {
        y <- character()
	for(i in seq_along(.romans)) {
	    d <- .romans[[i]]
            while(z >= d) {
                z <- z - d
                y <- c(y, romaNs[i])
            }
        }
        paste(y, collapse = "")
    }

    x <- as.integer(x)
    ind <- is.na(x) | (x <= 0L) | (x >= 3900L)
    out <- character(length(x))
    out[ind] <- NA
    out[!ind] <- vapply(x[!ind], n2r, "")
    out
}

.roman2numeric <- function(x)
{
    out <- integer(length(x))
    out[ina <- is.na(x) | !nzchar(x)] <- NA
    if(any(ind <- !ina)) {
        y <- toupper(x[ind])
        y <- gsub("CM", "DCCCC", y)
        y <- gsub("CD", "CCCC", y)
        y <- gsub("XC", "LXXXX", y)
        y <- gsub("XL", "XXXX", y)
        y <- gsub("IX", "VIIII", y)
        y <- gsub("IV", "IIII", y)
        ok <- grepl("^M{,3}D?C{,4}L?X{,4}V?I{,4}$", y)
        if(any(!ok)) {
            warning(sprintf(ngettext(sum(!ok),
                                     "invalid roman numeral: %s",
                                     "invalid roman numerals: %s"),
                            paste(x[ind][!ok], collapse = " ")),
                    domain = NA)
            out[ind][!ok] <- NA
        }
	out[ind][ok] <-
	    vapply(strsplit(y[ok], ""),
		   function(z)
		       as.integer(sum(.romans[match(z, names(.romans))])),
		   integer(1L))
    }
    out
}
