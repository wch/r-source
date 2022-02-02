#  File src/library/graphics/R/layout.R
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

## FIXME: Deprecate! - users *can* use  paste(.., "cm") themselves!
lcm <- function(x) paste(x, "cm")#-> 3 characters (used in layout!)

layout <-
    function(mat, widths = rep.int(1, ncol(mat)),
	     heights = rep.int(1, nrow(mat)), respect = FALSE)
{
    storage.mode(mat) <- "integer"
    mat <- as.matrix(mat) # or barf
    if(!is.logical(respect)) {
	respect <- as.matrix(respect)#or barf
	if(!is.matrix(respect) || any(dim(respect) != dim(mat)))
	    stop("'respect' must be logical or matrix with same dimension as 'mat'")
    }
    num.figures <- as.integer(max(mat))
    ## check that each value in 1..n is mentioned
    for (i in 1L:num.figures)
	if (match(i, mat, nomatch=0L) == 0L)
            stop(gettextf("layout matrix must contain at least one reference\nto each of the values {1 ... %d}\n",
                          num.figures), domain = NA)

    dm <- dim(mat)
    num.rows <- dm[1L]
    num.cols <- dm[2L]

    cm.widths  <- if (is.character(widths)) grep("cm", widths, fixed = TRUE)
    cm.heights <- if (is.character(heights)) grep("cm", heights, fixed = TRUE)

    ## pad widths/heights with 1's	and remove "cm" tags
    pad1.rm.cm <- function(v, cm.v, len) {
	if ((ll <- length(v)) < len)
	    v <- c(v, rep.int(1, len-ll))
	if (is.character(v)) {
	    wcm <- v[cm.v]
	    v[cm.v] <- substring(wcm, 1L, nchar(wcm, type="c") - 3)
            v <- chartr(getOption("OutDec"), ".", v)
	}
	as.numeric(v)
    }
    widths  <- pad1.rm.cm(widths, cm.widths,  len = num.cols)
    heights <- pad1.rm.cm(heights,cm.heights, len = num.rows)

    if (is.matrix(respect)) {
	respect.mat <- as.integer(respect)
	respect <- 2
    } else {# respect: logical	|--> 0 or 1
	respect.mat <- matrix(0L, num.rows, num.cols)
    }
    .External.graphics(C_layout,
                       num.rows, num.cols,
                       mat,# integer
                       as.integer(num.figures),
                       col.widths = widths,
                       row.heights = heights,
                       cm.widths,
                       cm.heights,
                       respect = as.integer(respect),
                       respect.mat)
    invisible(num.figures)
}

layout.show <- function(n=1)
{
    ## cheat to make sure that current plot is figure 1
    oma.saved <- par("oma")
    par(oma=rep.int(0,4))
    par(oma=oma.saved)

    o.par <- par(mar=rep.int(0,4))
    on.exit(par(o.par))
    for (i in seq_len(n)) {
	plot.new()
	box()
	text(0.5, 0.5, i)
    }
    invisible()
}
