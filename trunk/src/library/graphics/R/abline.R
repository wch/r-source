#  File src/library/graphics/R/abline.R
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

abline <- function(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
                   coef = NULL, untf = FALSE, ...)
{
    int_abline <- function(a, b, h, v, untf, col = par("col"),
                           lty = par("lty"), lwd = par("lwd"), ...)
        .External.graphics(C_abline, a, b, h, v, untf, col, lty, lwd, ...)

    if(!is.null(reg)) {
        if(!is.null(a)) warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if(is.object(a) || is.list(a)) {
	## was	(!is.null(a) && is.list(a))
	p <- length(coefa <- as.vector(coef(a)))
	if (p > 2)
            warning(gettextf("only using the first two of %d regression coefficients", p), domain = NA)
	islm <- inherits(a, "lm")
	noInt <- if(islm) !as.logical(attr(stats::terms(a), "intercept")) else p == 1
	if (noInt) {
	    a <- 0
	    b <- coefa[1L]
	} else {
	    a <- coefa[1L]
	    b <- if (p >= 2) coefa[2L] else 0
	}
    }
    if(!is.null(coef)) {
	if(!is.null(a)) warning("'a' and 'b' are overridden by 'coef'")
	a <- coef[1L]
	b <- coef[2L]
    }
    int_abline(a=a, b=b, h=h, v=v, untf=untf, ...)
    invisible()
}
