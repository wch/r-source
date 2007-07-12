# Copyright (C) 1997-1999  Adrian Trapletti
#
# Rewritten to use R indexing (C) 1999, 2006 R Core Development Team
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
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

embed <- function (x, dimension = 1)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        y <- matrix(0.0, n - dimension + 1, dimension * m)
        for (i in (1:m))
            y[, seq.int(i, by = m, length.out = dimension)] <-
                Recall (as.vector(x[,i]), dimension)
        return (y)
    } else if (is.vector(x) || is.ts(x)) {
        n <- length (x)
        if ((dimension < 1) | (dimension > n))
            stop ("wrong embedding dimension")
        m <- n - dimension + 1
        data <- x[1:m + rep.int(dimension:1, rep.int(m, dimension)) - 1]
        dim(data) <- c(m, dimension)
        return(data)
    } else
        stop ("'x' is not a vector or matrix")
}
