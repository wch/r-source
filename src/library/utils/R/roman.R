#  File src/library/utils/R/roman.R
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

as.roman <-
function(x)
{
    if(is.numeric(x))
        x <- as.integer(x)
    else if(is.character(x)) {
        ## Let's be nice: either strings that are *all* arabics, or
        ## (hopefully, for the time being) all romans.
        x <- if(all(grepl("^[[:digit:]]+$", x)))
            as.integer(x)
        else
            .roman2numeric(x)
    }
    else
        stop("cannot coerce 'x' to roman")
    x[(x <= 0L | x >= 3900L)] <- NA
    class(x) <- "roman"
    x
}

as.character.roman <-
function(x, ...)
    .numeric2roman(x)

format.roman <-
function(x, ...)
    format(as.character(x))

print.roman <-
function(x, ...)
{
    print(noquote(as.character(x)), ...)
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

.numeric2roman <-
function(x) {
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L,
                 5L, 4L, 1L)
    n2r <- function(z) {
        y <- character()
        for(i in seq_along(romans)) {
            d <- numbers[i]
            while(z >= d) {
                z <- z - d
                y <- c(y, romans[i])
            }
        }
        paste(y, collapse = "")
    }

    out <- character(length(x))
    x <- as.integer(x)
    ind <- is.na(x) | (x <= 0L) | (x >= 3900L)
    out[ind] <- NA
    if(any(!ind))
        out[!ind] <- sapply(x[!ind], n2r)
    out
}

.roman2numeric <-
function(x)
{
    ## <FIXME>
    ## What if this fails?
    ## Should say something like "Not a valid roman number ..."
    ## </FIXME>
    romans <- c("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
                "V", "IV", "I")
    numbers <- c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 9L,
                 5L, 4L, 1L)
    out <- integer(length(x))
    ind <- is.na(x)
    out[ind] <- NA
    if(any(!ind)) {
        y <- toupper(x[!ind])
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
                            paste(x[!ind][!ok], collapse = " ")),
                    domain = NA)
            out[!ind][!ok] <- NA
        }
        if(any(ok))
            out[!ind][ok] <-
                sapply(strsplit(y[ok], ""),
                       function(z)
                       as.integer(sum(numbers[match(z, romans)])))
    }
    out
}
