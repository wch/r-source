#  File src/library/base/R/interaction.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

### This is almost like the Primitive ":" for factors
### but with drop=TRUE, used in reshape and ave
interaction <- function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg < 1L)
	stop("No factors specified")
    if (narg == 1L && is.list(args[[1L]])) {
	args <- args[[1L]]
	narg <- length(args)
    }
    for(i in narg:1L) {
        x <- as.factor(args[[i]])[, drop = drop]
        ax <- as.integer(x) - 1L
        lx <- levels(x)
        if(i == narg) {
            ay <- ax
            ly <- lx
        } else {
            nx <- length(lx)
            ny <- length(ly)
            if(lex.order) {
                ay <- ay + ny * ax
                if(drop) {
                    az <- sort(unique(ay))
                    ly <- paste(lx[az %/% ny + 1L], ly[az %% ny + 1L],
                                sep = sep)
                    ay <- match(ay, az) - 1L
                } else {
                    ly <- paste(rep(lx, each = ny), rep(ly, nx),
                                sep = sep)
                }
            } else {
                ay <- ay * nx + ax
                if(drop) {
                    az <- sort(unique(ay))
                    ly <- paste(lx[az %% nx + 1L], ly[az %/% nx + 1L],
                                sep = sep)
                    ay <- match(ay, az) - 1L
                } else {
                    ly <- paste(rep(lx, ny), rep(ly, each = nx),
                                sep = sep)
                }
            }
            while(j <- anyDuplicated(ly)) {
                ## If levels at positions i and j > i are the same, we
                ## need to drop the one at j, change the code for that
                ## level to the code for level i, and decrease all codes
                ## beyond the code for level j by one.
                i <- match(ly[j], ly)
                ly <- ly[-j]
                j <- j - 1L
                ay[ay == j] <- i - 1L
                ay[ay > j] <- ay[ay > j] - 1L
            }
        }
    }
    structure(as.integer(ay + 1L), levels = ly, class = "factor")
}
