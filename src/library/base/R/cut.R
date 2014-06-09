#  File src/library/base/R/cut.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

cut <- function(x, ...) UseMethod("cut")

cut.default <-
    function (x, breaks, labels = NULL, include.lowest = FALSE,
              right = TRUE, dig.lab = 3L, ordered_result = FALSE, ...)
{
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (length(breaks) == 1L) {
	if (is.na(breaks) || breaks < 2L)
	    stop("invalid number of intervals")
	nb <- as.integer(breaks + 1) # one more than #{intervals}
	dx <- diff(rx <- range(x, na.rm = TRUE))
	if(dx == 0) {
            dx <- abs(rx[1L])
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                              length.out = nb)
        } else {
            breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
            breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] + dx/1000)
        }
    } else nb <- length(breaks <- sort.int(as.double(breaks)))
    if (anyDuplicated(breaks)) stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {#- try to construct nice ones ..
	for(dig in dig.lab:max(12L, dig.lab)) {
            ## 0+ avoids printing signed zeros as "-0"
	    ch.br <- formatC(0+breaks, digits = dig, width = 1L,
                             decimal.mark = getOption("OutDec"))
	    if(ok <- all(ch.br[-1L] != ch.br[-nb])) break
	}
	labels <-
	    if(ok) paste0(if(right)"(" else "[",
                          ch.br[-nb], ",", ch.br[-1L],
                          if(right)"]" else ")")
	    else paste("Range", seq_len(nb - 1L), sep="_")
        if (ok && include.lowest) {
            if (right)
                substr(labels[1L], 1L, 1L) <- "[" # was "("
            else
                substring(labels[nb-1L],
                          nchar(labels[nb-1L], "c")) <- "]" # was ")"
        }
    } else if (is.logical(labels) && !labels)
        codes.only <- TRUE
    else if (length(labels) != nb - 1L)
        stop("lengths of 'breaks' and 'labels' differ")
    code <- .bincode(x, breaks, right, include.lowest)
    if(codes.only) code
    else factor(code, seq_along(labels), labels, ordered = ordered_result)
}

## called from image.default and for use in packages.
.bincode <- function(x, breaks, right = TRUE, include.lowest = FALSE)
    .Internal(bincode(x, breaks, right, include.lowest))

