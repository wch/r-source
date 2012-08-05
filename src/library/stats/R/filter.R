#  File src/library/stats/R/filter.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-2012 The R Core Team
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

filter <- function(x, filter, method = c("convolution", "recursive"),
                   sides = 2L, circular = FALSE, init=NULL)
{
    method <- match.arg(method)
    x <- as.ts(x)
    xtsp <- tsp(x)
    x <- as.matrix(x)
    n <- as.integer(nrow(x))
    if (is.na(n)) stop("invalid value of nrow(x)", domain = NA)
    nser <- ncol(x)
    nfilt <- as.integer(length(filter))
    if (is.na(n)) stop("invalid value of length(filter)", domain = NA)
   if(any(is.na(filter))) stop("missing values in 'filter'")
    y <- matrix(NA, n, nser)
    if(method == "convolution") {
        if(nfilt > n) stop("'filter' is longer than time series")
        sides <- as.integer(sides)
        if(is.na(sides) ||( sides != 1 && sides != 2))
            stop("argument 'sides' must be 1 or 2")
        circular <- as.logical(circular)
        if (is.na(circular)) stop("'circular' must be logical and not NA")
        for (i in 1L:nser)
            y[, i] <- .C(C_filter1,
                         as.double(x[,i]),
                         n,
                         as.double(filter),
                         nfilt,
                         sides,
                         circular,
                         out = double(n), NAOK = TRUE,
                         PACKAGE = "stats")$out
    } else {
        if(missing(init)) {
            init <- matrix(0, nfilt, nser)
        } else {
            ni <- NROW(init)
            if(ni != nfilt)
                stop("length of 'init' must equal length of 'filter'")
            if(NCOL(init) != 1 && NCOL(init) != nser)
                stop(gettextf("'init'; must have 1 or %d cols", nser),
                     domain = NA)
            if(!is.matrix(init)) init <- matrix(init, nfilt, nser)
        }
        for (i in 1L:nser)
            y[, i] <- .C(C_filter2,
                         as.double(x[,i]),
                         n,
                         as.double(filter),
                         nfilt,
                         out = as.double(c(rev(init[, i]), double(n))),
                         NAOK = TRUE,
                         PACKAGE = "stats")$out[-(1L:nfilt)]
    }
    y <- drop(y)
    tsp(y) <- xtsp
    class(y) <- if(nser > 1) c("mts", "ts") else "ts"
    y
}

