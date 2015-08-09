#  File src/library/stats/R/qqnorm.R
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

qqnorm <- function(y, ...) UseMethod("qqnorm")

qqnorm.default <-
    function(y, ylim, main = "Normal Q-Q Plot",
	     xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
	     plot.it = TRUE, datax = FALSE, ...)
{
    if(has.na <- any(ina <- is.na(y))) { ## keep NA's in proper places
        yN <- y
        y <- y[!ina]
    }
    if(0 == (n <- length(y)))
        stop("y is empty or has only NAs")
    if (plot.it && missing(ylim))
        ylim <- range(y)
    x <- qnorm(ppoints(n))[order(order(y))]
    if(has.na) {
        y <- x; x <- yN; x[!ina] <- y
        y <- yN
    }
    if(plot.it)
        if (datax)
            plot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim, ...)
        else
            plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
    invisible(if(datax) list(x = y, y = x) else list(x = x, y = y))
}

## Splus also has qqnorm.aov(), qqnorm.aovlist(), qqnorm.maov() ...

qqline <- function(y, datax = FALSE, distribution = qnorm,
                   probs = c(0.25, 0.75), qtype = 7, ...)
{
    stopifnot(length(probs) == 2, is.function(distribution))
    y <- quantile(y, probs, names=FALSE, type=qtype, na.rm = TRUE)
    x <- distribution(probs)
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1L] - slope*y[1L]
    } else {
        slope <- diff(y)/diff(x)
        int <- y[1L]-slope*x[1L]
    }
    abline(int, slope, ...)
}
