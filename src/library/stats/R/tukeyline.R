#  File src/library/stats/R/tukeyline.R
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

line <- function(x, y=NULL)
{
    xy <- xy.coords(x, y)
    ok <- complete.cases(xy$x,xy$y)
    n <- length(ok)
    if(n <= 1) stop("insufficient observations")
    z <- .C("tukeyline",
	    as.double(xy$x[ok]),
	    as.double(xy$y[ok]),
	    double(n),
	    double(n),
	    n,
	    double(2),
	    DUP=FALSE, PACKAGE="stats")
    value <- list(call=sys.call(), coefficients = z[[6L]],
                  residuals = z[[3L]], fitted.values = z[[4L]])
    class(value) <- "tukeyline"
    value
}
#coef.tukeyline <- coef.lm
residuals.tukeyline <- residuals.lm
# fitted.tukeyline <- fitted.lm
print.tukeyline <- print.lm
