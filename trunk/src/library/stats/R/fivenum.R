#  File src/library/stats/R/fivenum.R
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

fivenum <- function(x, na.rm=TRUE)
{
    xna <- is.na(x)
    if(any(xna)) {
        if(na.rm) x <- x[!xna]
        else return(rep.int(NA,5))
    }
    x <- sort(x)
    n <- length(x)
    if(n == 0) rep.int(NA,5)
    else {
        n4 <- floor((n+3)/2) / 2
	d <- c(1, n4, (n+1)/2, n + 1 - n4, n)
	0.5*(x[floor(d)] + x[ceiling(d)])
    }
}
