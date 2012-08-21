#  File src/library/stats/R/tukeyline.R
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
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

line <- function(x, y=NULL)
{
    xy <- xy.coords(x, y)
    ok <- complete.cases(xy$x,xy$y)
    n <- as.integer(length(ok))
    if(is.na(n)) stop("invalid length(x)")
    if(n <= 1L) stop("insufficient observations")
    z <- .C(C_tukeyline,
	    as.double(xy$x[ok]),
	    as.double(xy$y[ok]),
	    double(n),
	    double(n),
	    n,
	    double(2),
	    DUP = FALSE)
    value <- list(call = sys.call(), coefficients = z[[6L]],
                  residuals = z[[3L]], fitted.values = z[[4L]])
    class(value) <- "tukeyline"
    value
}
#coef.tukeyline <- coef.lm
residuals.tukeyline <- residuals.lm
# fitted.tukeyline <- fitted.lm
print.tukeyline <- print.lm
