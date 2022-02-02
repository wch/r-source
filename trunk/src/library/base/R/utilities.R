#  File src/library/base/R/utilities.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

mat.or.vec <- function(nr,nc)
    if(nc == 1L) numeric(nr) else matrix(0, nr, nc)

## Use  'version' since that exists in all S dialects :
is.R <-
    function() exists("version") && !is.null(vl <- version$language) && vl == "R"

## deparse(.) returning *one* string
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...)
    paste(deparse(expr, width.cutoff, ...), collapse=collapse)

