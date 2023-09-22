#  File src/library/base/R/range.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

### MM: ........ *OR*  introduce an  allow.infinite()  generic (semantically <==> allow.finite() )
###     as Davis proposes on May 9 on the R-devel mailing list

.rangeNum <- function(..., na.rm, finite, isNumeric)
{
    x <- c(..., recursive = TRUE)
    if(isNumeric(x)) {
        if(finite) x <- x[is.finite(x)]
        else if(na.rm) x <- x[!is.na(x)]
	c(min(x), max(x))
    } else {
        if(finite) na.rm <- TRUE
        c(min(x, na.rm=na.rm), max(x, na.rm=na.rm))
    }
}

range.default <- function(..., na.rm = FALSE, finite = FALSE)
    .rangeNum(..., na.rm=na.rm, finite=finite, isNumeric = is.numeric)

range.POSIXct <- range.Date <- function(..., na.rm = FALSE, finite = FALSE)
    .rangeNum(..., na.rm=na.rm, finite=finite, isNumeric = function(.)TRUE)
