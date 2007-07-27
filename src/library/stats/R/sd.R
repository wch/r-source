#  File src/library/stats/R/sd.R
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

sd <- function(x, na.rm=FALSE) {
    if (is.matrix(x))
	apply(x, 2, sd, na.rm=na.rm)
    else if (is.vector(x))
	sqrt(var(x, na.rm=na.rm))
    else if (is.data.frame(x))
	sapply(x, sd, na.rm=na.rm)
    else 
	sqrt(var(as.vector(x), na.rm=na.rm))
}
