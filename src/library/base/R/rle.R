#  File src/library/base/R/rle.R
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

rle <- function(x)
{
    if (!is.vector(x) && !is.list(x))
        stop("'x' must be an atomic vector")
    n <- length(x)
    if (n == 0)
        return(list(lengths = integer(0), values = x))
    y <- x[-1] != x[-n]
    i <- c(which(y | is.na(y)), n)
    structure(list(lengths = diff(c(0L, i)), values = x[i]),
              class = "rle")
}

print.rle <- function(x, digits = getOption("digits"), ...)
{
    if(is.null(digits)) digits <- getOption("digits")
    cat("Run Length Encoding\n  lengths:")
    utils::str(x$lengths)
    cat("  values :")
    utils::str(x$values, digits.d = digits)
    invisible(x)
}

inverse.rle <- function(x, ...)
{
    if(is.null(le <- x$lengths) ||
       is.null(v  <- x$values) || length(le) != length(v))
        stop("invalid 'rle' structure")
    rep(v, le)
}

