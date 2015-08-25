#  File src/library/stats/R/embed.R
#  Part of the R package, https://www.R-project.org
#
# Copyright (C) 1997-1999  Adrian Trapletti
#
# Rewritten to use R indexing (C) 1999, 2006 R Core Team
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, a copy is available at
# https://www.R-project.org/Licenses/

embed <- function (x, dimension = 1)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        y <- matrix(0.0, n - dimension + 1L, dimension * m)
        for (i in seq_len(m))
            y[, seq.int(i, by = m, length.out = dimension)] <-
                Recall (as.vector(x[,i]), dimension)
        return (y)
    } else if (is.vector(x) || is.ts(x)) {
        n <- length (x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        m <- n - dimension + 1L
        data <- x[1L:m + rep.int(dimension:1L, rep.int(m, dimension)) - 1L]
        dim(data) <- c(m, dimension)
        return(data)
    } else
        stop ("'x' is not a vector or matrix")
}
