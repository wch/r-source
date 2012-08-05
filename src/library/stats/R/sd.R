#  File src/library/stats/R/sd.R
#  Part of the R package, http://www.R-project.org
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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## in the future:
sd <- function(x, na.rm=FALSE)	sqrt(var(as.double(x), na.rm=na.rm))

sd <- function(x, na.rm=FALSE) {
    if (is.matrix(x)) {
	msg <- "sd(<matrix>) is deprecated.\n Use apply(*, 2, sd) instead."
	warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
	apply(x, 2, sd, na.rm=na.rm)
    }
    else if (is.vector(x))
	sqrt(var(x, na.rm=na.rm))
    else if (is.data.frame(x)) {
	msg <- "sd(<data.frame>) is deprecated.\n Use sapply(*, sd) instead."
	warning(paste(msg, collapse = ""), call. = FALSE, domain = NA)
	sapply(x, sd, na.rm=na.rm)
    }
    else
	sqrt(var(as.vector(x), na.rm=na.rm))
}
