#  File src/library/base/R/is.R
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

is.vector <- function(x, mode="any") .Internal(is.vector(x,mode))

`is.na<-` <- function(x, value) UseMethod("is.na<-")

`is.na<-.default` <- function(x, value)
{
    x[value] <- NA
    x
}

is.primitive <- function(x)
    switch(typeof(x), "special" = , "builtin" = TRUE, FALSE)
