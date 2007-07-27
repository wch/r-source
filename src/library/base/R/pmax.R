#  File src/library/base/R/pmax.R
#  Part of the R package, http://www.R-project.org
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

pmin.int <- function(..., na.rm = FALSE) .Internal(pmin(na.rm, ...))
pmax.int <- function(..., na.rm = FALSE) .Internal(pmax(na.rm, ...))

pmax <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0) stop("no arguments")
    if(all(sapply(elts, function(x) is.atomic(x) && !is.object(x)))) {
        ## NB: NULL passes is.atomic
        mmm <- .Internal(pmax(na.rm, ...))
    } else {
        mmm <- elts[[1]]
        attr(mmm, "dim") <- NULL  # dim<- would drop names
        has.na <- FALSE
        for (each in elts[-1]) {
            attr(each, "dim") <- NULL
            if(length(mmm) < (m <- length(each)))
                mmm <- rep(mmm, length.out=m)
            else if(length(each) < (m <- length(mmm)))
                each <- rep(each, length.out=m)
            nas <- cbind(is.na(mmm), is.na(each))
            if(has.na || (has.na <- any(nas))) {
                mmm[nas[,1]] <- each[nas[,1]]
                each[nas[,2]] <- mmm[nas[,2]]
            }
            change <- mmm < each
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) mmm[nas[,1] | nas[,2]] <- NA
        }
    }
    mostattributes(mmm) <- attributes(elts[[1]])
    mmm
}

pmin <- function (..., na.rm = FALSE)
{
    elts <- list(...)
    if(length(elts) == 0) stop("no arguments")
    if(all(sapply(elts, function(x) is.atomic(x) && !is.object(x)))) {
        mmm <- .Internal(pmin(na.rm, ...))
    } else {
        mmm <- elts[[1]]
        attr(mmm, "dim") <- NULL  # dim<- would drop names
        has.na <- FALSE
        for (each in elts[-1]) {
            attr(each, "dim") <- NULL
            if(length(mmm) < (m <- length(each)))
                mmm <- rep(mmm, length.out=m)
            else if(length(each) < (m <- length(mmm)))
                each <- rep(each, length.out=m)
            nas <- cbind(is.na(mmm), is.na(each))
            if(has.na || (has.na <- any(nas))) {
                mmm[nas[,1]] <- each[nas[,1]]
                each[nas[,2]] <- mmm[nas[,2]]
            }
            change <- mmm > each
            change <- change & !is.na(change)
            mmm[change] <- each[change]
            if (has.na && !na.rm) mmm[nas[,1] | nas[,2]] <- NA
        }
    }
    mostattributes(mmm) <- attributes(elts[[1]])
    mmm
}
