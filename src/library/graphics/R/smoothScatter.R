#  File src/library/graphics/R/smoothScatter.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


smoothScatter <- function(x, y=NULL, nbin=128, bandwidth,
			  colramp=colorRampPalette(c("white", blues9)),
			  nrpoints=100, ret.selection=FALSE, pch=".", cex=1, col="black",
			  transformation = function(x) x^.25,
                          postPlotHook = box,
			  xlab=NULL, ylab=NULL, xlim, ylim,
			  xaxs=par("xaxs"), yaxs=par("yaxs"), ...)
{
    if (!is.numeric(nrpoints) || nrpoints < 0 || length(nrpoints) != 1)
	stop("'nrpoints' should be numeric scalar with value >= 0.")
    nrpoints <- round(nrpoints)
    ret.selection <- ret.selection && nrpoints > 0

    ## similar as in plot.default
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel)
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab

    ## eliminate non-finite (incl. NA) values
    ## want to keep rownames
    x <- cbind(xy$x, xy$y)[ I <- is.finite(xy$x) & is.finite(xy$y), , drop=FALSE]
    if(ret.selection) iS <- which(I)

    ## xlim and ylim
    if(!missing(xlim)) {
	stopifnot(is.numeric(xlim), length(xlim)==2, is.finite(xlim))
	x <- x[ I <- min(xlim) <= x[,1] & x[,1] <= max(xlim), , drop=FALSE]
        if(ret.selection) iS <- iS[I]
    } else {
	xlim <- range(x[,1])
    }
    if(!missing(ylim)) {
	stopifnot(is.numeric(ylim), length(ylim)==2, is.finite(ylim))
	x <- x[ I <- min(ylim) <= x[,2] & x[,2] <= max(ylim), , drop=FALSE]
        if(ret.selection) iS <- iS[I]
    } else {
	ylim <- range(x[,2])
    }

    ## create density map [ code in --> ../../grDevices/R/smooth2d.R ]:
    map	 <- grDevices:::.smoothScatterCalcDensity(x, nbin, bandwidth)
    xm	 <- map$x1
    ym	 <- map$x2
    dens <- map$fhat
    dens[] <- transformation(dens)

    ## plot color image
    image(xm, ym, z=dens, col=colramp(256),
          xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
    if(!is.null(postPlotHook))
	postPlotHook()

    ## plot selection of dots
    if (nrpoints > 0) {
	nrpoints <- min(nrow(x), ceiling(nrpoints))
	## we assume that map$x1 and map$x2 go linearly from
	## their first to their last value in nbin steps
	stopifnot((nx <- length(xm)) == nrow(dens),
		  (ny <- length(ym)) == ncol(dens))
	ixm <- 1L + as.integer((nx-1)*(x[,1]-xm[1])/(xm[nx]-xm[1]))
	iym <- 1L + as.integer((ny-1)*(x[,2]-ym[1])/(ym[ny]-ym[1]))
	sel <- order(dens[cbind(ixm, iym)])[seq_len(nrpoints)]
	x <- x[sel, , drop=FALSE]
	points(x, pch=pch, cex=cex, col=col)
        if(ret.selection) iS[sel]
    }
}
