#  File src/library/base/R/identical.R
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

identical <- function(x, y, num.eq = TRUE, single.NA = TRUE,
                      attrib.as.set = TRUE, ignore.bytecode = TRUE,
                      ignore.environment = FALSE, ignore.srcref = TRUE)
    .Internal(identical(x,y, num.eq, single.NA, attrib.as.set,
                        ignore.bytecode, ignore.environment, ignore.srcref))

## till R 3.4.x:
## isTRUE <- function(x) identical(TRUE, x)

## The following _fails_ e.g., when 'x' is a function or environment:
## isTRUE  <- function(x) identical(TRUE, as.vector(x))

## NB:  is.logical(.) will never dispatch:
## --                 base::is.logical(x)  <==>  typeof(x) == "logical"
isTRUE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && x
isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

