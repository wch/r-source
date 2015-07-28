#  File src/library/stats/R/filter.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-2014 The R Core Team
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
    storage.mode(x) <- "double"
    xtsp <- tsp(x)
    n <- as.integer(NROW(x))
    if (is.na(n)) stop("invalid value of nrow(x)", domain = NA)
    nser <- NCOL(x)
    filter <- as.double(filter)
    nfilt <- as.integer(length(filter))
    if (is.na(n)) stop("invalid value of length(filter)", domain = NA)
    if(anyNA(filter)) stop("missing values in 'filter'")

    if(method == "convolution") {
        if(nfilt > n) stop("'filter' is longer than time series")
        sides <- as.integer(sides)
        if(is.na(sides) || (sides != 1L && sides != 2L))
            stop("argument 'sides' must be 1 or 2")
        circular <- as.logical(circular)
        if (is.na(circular)) stop("'circular' must be logical and not NA")
        if (is.matrix(x)) {
            y <- matrix(NA, n, nser)
            for (i in seq_len(nser))
                y[, i] <- .Call(C_cfilter, x[, i], filter, sides, circular)
        } else
            y <- .Call(C_cfilter, x, filter, sides, circular)
    } else {
        if(missing(init)) {
            init <- matrix(0, nfilt, nser)
        } else {
            ni <- NROW(init)
            if(ni != nfilt)
                stop("length of 'init' must equal length of 'filter'")
            if(NCOL(init) != 1L && NCOL(init) != nser) {
                stop(sprintf(ngettext(nser,
                                      "'init' must have %d column",
                                      "'init' must have 1 or %d columns",
                                      domain = "R-stats"),
                             nser), domain = NA)
            }
            if(!is.matrix(init)) dim(init) <- c(nfilt, nser)
        }
        ind <- seq_len(nfilt)
        ## NB: this .Call alters its third argument
        if (is.matrix(x)) {
            y <- matrix(NA, n, nser)
            for (i in seq_len(nser))
                y[, i] <-
                    .Call(C_rfilter, x[, i], filter,
                          c(rev(init[, i]), double(n)))[-ind]
        } else
                y <-
                    .Call(C_rfilter, x, filter,
                          c(rev(init[, 1L]), double(n)))[-ind]

    }
#    y <- drop(y)
    tsp(y) <- xtsp
    class(y) <- if(nser > 1L) c("mts", "ts") else "ts"
    y
}

