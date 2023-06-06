#  File src/library/stats/R/na.ts.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

na.contiguous <- function(object, ...) UseMethod("na.contiguous")

na.contiguous.default <- function(object, doTsp = !is.null(attr(object, "tsp")), ...)
{
    if(!length(object)) return(object)
    tm <- time(object)
    xfreq <- frequency(object)
    ## use (first) maximal contiguous length of non-NAs
    if(is.matrix(object))
        good <- apply(!is.na(object), 1L, all)
    else  good <- !is.na(object)
    if(!sum(good)) stop("all times contain an NA")
    tt <- c(0L, cumsum(!good))
    ln <- sapply(0:max(tt), function(i) sum(tt==i))
    seg <- (seq_along(ln)[ln==max(ln)])[1L] - 1L
    keep <- (tt == seg)[-1L]
    st <- min(which(keep))
    if(!good[st]) st <- st + 1L
    en <- max(which(keep))
    omit <- integer()
    n <- NROW(object)
    if(st > 1L) omit <- c(omit, 1L:(st-1L))
    if(en < n ) omit <- c(omit, (en+1L):n)
    if(length(omit)) {
        cl <- class(object) ; force(doTsp) # before modification
        object <- if(is.matrix(object)) object[st:en,] else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        if(doTsp) # e.g., not if it was a simple vector/matrix
            tsp(object) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(object) <- cl
    }
    object
}
