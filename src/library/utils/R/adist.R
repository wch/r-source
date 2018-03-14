#  File src/library/utils/R/adist.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

adist <-
function(x, y = NULL, costs = NULL, counts = FALSE, fixed = TRUE,
         partial = !fixed, ignore.case = FALSE, useBytes = FALSE)
{
    bytesToInt <- function(x) {
        if(is.na(x)) return(NA_integer_)
        as.integer(charToRaw(x))
    }

    costs <- .amatch_costs(costs)

    nmx <- names(x)
    x <- as.character(x)
    names(x) <- nmx

    if(!is.null(y)) {
        nmy <- names(y)
        y <- as.character(y)
        names(y) <- nmy
    }

    if(!isFALSE(fixed) && !isTRUE(partial)) {
        ex <- Encoding(x)
        useBytes <- isTRUE(useBytes) || any(ex == "bytes")
        if(!is.null(y)) {
            ey <- Encoding(y)
            useBytes <- useBytes || any(ey == "bytes")
        }
        if(useBytes) {
            x <- lapply(x, bytesToInt)
            y <- if(is.null(y)) {
                x
            } else {
                lapply(y, bytesToInt)
            }
        } else {
            ignore.case <- isTRUE(ignore.case)
            x <- if(ignore.case) {
                lapply(tolower(enc2utf8(x)), utf8ToInt)
            } else {
                lapply(enc2utf8(x), utf8ToInt)
            }
            y <- if(is.null(y)) {
                x
            } else if(ignore.case) {
                lapply(tolower(enc2utf8(y)), utf8ToInt)
            } else {
                lapply(enc2utf8(y), utf8ToInt)
            }
        }
    }
    else {
        if(is.null(y)) {
            y <- x
        }
        ## TRE needs integer costs: coerce here for simplicity.
        costs <- as.integer(costs)
    }

    .Internal(adist(x, y, costs, counts, fixed, partial, ignore.case,
                    useBytes))
}

aregexec <-
function(pattern, text, max.distance = 0.1, costs = NULL,
         ignore.case = FALSE, fixed = FALSE, useBytes = FALSE)
{
    ## TRE needs integer costs: coerce here for simplicity.
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)

    .Internal(aregexec(as.character(pattern),
                       as.character(text),
                       bounds, costs, ignore.case, fixed, useBytes))
}

## No longer used by adist(), but could be more generally useful ...

regquote <-
function(x)
    gsub("([*.?+^&\\[])", "\\\\\\1", x)

