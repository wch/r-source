#  File src/library/base/R/sort_by.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2024 The R Core Team
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


sort_by <- function(x, y, ...)
{
    UseMethod("sort_by")
}

sort_by.default <- function(x, y, ...)
{
    x[order(y, ...)]
}


## data.frame method. Supported usage patterns:
##
## sort_by(mtcars, mtcars$am)
## sort_by(mtcars, list(mtcars$am, -mtcars$mpg))
## sort_by(mtcars, ~ list(am, -mpg))
## sort_by(mtcars, ~ am + mpg)
## sort_by(mtcars, ~ am + I(-mpg))

## Maybe (in future)
## sort_by(mtcars, ~ am - mpg) equivalent to ~ am + I(-mpg)


sort_by.data.frame <- function(x, y, ...)
{
    if (inherits(y, "formula")) y <- .formula2varlist(y, x)
    if (!is.list(y)) y <- list(y)
    o <- do.call(order, c(unname(y), list(...)))
    x[o, , drop = FALSE]
}
