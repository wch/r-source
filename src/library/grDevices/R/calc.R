#  File src/library/grDevices/R/calc.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

#### Functions that calculate useful stuff for plotting
#### BUT which do not do any actual drawing
#### Useful for both graphics and grid to have access to

boxplot.stats <- function(x, coef = 1.5, do.conf = TRUE, do.out = TRUE)
{
    if(coef < 0) stop("'coef' must not be negative")
    nna <- !is.na(x)
    n <- sum(nna)                       # including +/- Inf
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    if(coef == 0)
	do.out <- FALSE
    else { ## coef > 0
	out <- if(!is.na(iqr)) { x < (stats[2L] - coef * iqr) |
				 x > (stats[4L] + coef * iqr)
			     } else !is.finite(x)
	if(any(out[nna], na.rm = TRUE))
	    stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    }
    conf <- if(do.conf) stats[3L] + c(-1.58, 1.58) * iqr / sqrt(n)
    list(stats = stats, n = n, conf = conf,
	 out = if(do.out) x[out & nna] else numeric())
}

## Contour lines
contourLines <-
function (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)),
	  z, nlevels = 10, levels = pretty(range(z, na.rm = TRUE), nlevels))
{
    ## FIXME: This "validation" code for the x, y, z values
    ## should be put in a function for contourLines, contour,
    ## image (and persp?) to share.  Unfortunately, an xyz.coords
    ## already exists which isn't really compatible with the
    ## desired behaviour here.
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		z <- x
		x <- seq.int(0, 1, length.out = nrow(z))
	    }
	} else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
	y <- x$y
	x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
	stop("no proper 'z' matrix specified")
    if (1.0 * length(x) * length(y) != length(z))
        stop("dimensions of 'x', 'y' and 'z' do not match")
    invisible(.External2(C_contourLines, x, y, z, levels))
}

chull <- function(x, y = NULL)
{
    X <- xy.coords(x, y, recycle = TRUE)
    x <- cbind(X$x, X$y)
    if(any(!is.finite(x))) stop("finite coordinates are needed")
    if(nrow(x) == 0) return(integer())
    if(nrow(x) == 1) return(1L)
    res <- .Call(C_chull, x)
    ## if this is called on multiple copies of a single point
    ## res is of length one.
    if (length(res) < 2L) return(res)
    ## fix up order: needed in rare cases: PR#15127
    xx <- sweep(x[res, ], 2L, colMeans(x[res, ]))
    angs <- atan2(xx[, 2L], -xx[, 1L])
    res[order(angs)]
}

nclass.Sturges <- function(x) ceiling(log2(length(x)) + 1)

nclass.scott <- function(x)
{
    h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1/3)
    if(h > 0) ceiling(diff(range(x))/h) else 1L
}

nclass.FD <- function(x)
{
    h <- stats::IQR(x)
    if(h == 0) h <- stats::mad(x, constant = 2) # c=2: consistent with IQR
    if (h > 0) ceiling(diff(range(x))/(2 * h * length(x)^(-1/3))) else 1L
}


## Sunflower Plot computation:
## Used to be part of ../../graphics/R/sunflowerplot.R :
xyTable <- function(x, y = NULL, digits)
{
    ## Compute number := multiplicities of (x[i], y[i])

    x <- xy.coords(x, y)

    ## get rid of rounding fuzz:
    y <- signif(x$y, digits=digits)
    x <- signif(x$x, digits=digits)
    n <- length(x)
    number <-
	if(n > 0) {
	    orderxy <- order(x, y)
	    x <- x[orderxy]
	    y <- y[orderxy]
	    first <- c(TRUE, (x[-1L] != x[-n]) | (y[-1L] != y[-n]))
	    x <- x[first]
	    y <- y[first]
	    diff(c((1L:n)[first], n + 1L))
	}
	else integer()

    list(x = x, y = y, number = number)
}

axisTicks <- function(usr, log, axp = NULL, nint = 5) {
    if(is.null(axp))
	axp <- unlist(.axisPars(usr, log=log, nintLog=nint), use.names=FALSE)
    .Call(C_R_CreateAtVector, axp, if(log) 10^usr else usr, nint, log)
}

.axisPars <- function(usr, log = FALSE, nintLog = 5) {
    .Call(C_R_GAxisPars, usr, log, nintLog)
}
