#  File src/library/stats/R/mad.R
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

mad <- function(x, center = median(x), constant = 1.4826,
                na.rm = FALSE, low = FALSE, high = FALSE)
{
    if(na.rm)
	x <- x[!is.na(x)]
    n <- length(x)
    constant *
        if((low || high) && n%%2 == 0) {
            if(low && high) stop("'low' and 'high' cannot be both TRUE")
            n2 <- n %/% 2 + as.integer(high)
            sort(abs(x - center), partial = n2)[n2]
        }
        else median(abs(x - center))
}

