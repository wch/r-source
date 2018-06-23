#  File src/library/stats/R/weighted.mean.R
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

weighted.mean <- function(x, w, ...) UseMethod("weighted.mean")

weighted.mean.default <- function(x, w, ..., na.rm = FALSE)
{
    if(missing(w)) {
        ## avoid creating weights vector
        if (na.rm) x <- x[!is.na(x)]
        return(sum(x)/length(x))
    }
    if (length(w) != length(x))
        stop("'x' and 'w' must have the same length")
    if (na.rm) { i <- !is.na(x); w <- w[i]; x <- x[i] }
    sum((x*w)[w != 0])/sum(w) # --> NaN in empty case
}

## see note for ?mean.Date
weighted.mean.Date <- function (x, w, ...)
    structure(weighted.mean(unclass(x), w, ...), class = "Date")

weighted.mean.POSIXct <- function (x, w, ...)
    .POSIXct(weighted.mean(unclass(x), w, ...), attr(x, "tzone"))

weighted.mean.POSIXlt <- function (x, w, ...)
    as.POSIXlt(weighted.mean(as.POSIXct(x), w, ...))

weighted.mean.difftime <- function (x, w, ...)
    structure(weighted.mean(unclass(x), w, ...),
              units = attr(x, "units"), class = "difftime")
