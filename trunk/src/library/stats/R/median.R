#  File src/library/stats/R/median.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

median <- function(x, na.rm=FALSE, ...) UseMethod("median")

median.default <- function(x, na.rm = FALSE, ...)
{
    if(is.factor(x) || is.data.frame(x)) stop("need numeric data")
    ## all other objects only need is.na(), sort(), mean(), [() to be working
    if(length(names(x))) names(x) <- NULL # for e.g., c(x = NA_real_)
##    if(na.rm) x <- x[!is.na(x)] else if(anyNA(x)) return(x[FALSE][NA])
    if(na.rm) x <- x[!is.na(x)] else if(any(is.na(x))) return(x[FALSE][NA])
    n <- length(x)
    if (n == 0L) return(x[FALSE][NA])
    half <- (n + 1L) %/% 2L
    if(n %% 2L == 1L) sort(x, partial = half)[half]
    else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}
