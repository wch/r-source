#  File src/library/stats/R/cancor.R
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

## Seber pages 506-507, after a Golub original

cancor <- function(x, y, xcenter=TRUE, ycenter=TRUE)
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if((nr <- nrow(x)) != nrow(y)) stop("unequal number of rows in 'cancor'")
    ncx <- ncol(x)
    ncy <- ncol(y)
    if(!nr || !ncx || !ncy) stop("dimension 0 in 'x' or 'y'")
    if(is.logical(xcenter)) {
	if(xcenter) {
	    xcenter <- colMeans(x,)
	    x <- x - rep(xcenter, rep.int(nr, ncx))
	}
	else xcenter <- rep.int(0, ncx)
    }
    else {
	xcenter <- rep_len(xcenter, ncx)
	x <- x - rep(xcenter, rep.int(nr, ncx))
    }
    if(is.logical(ycenter)) {
	if(ycenter) {
	    ycenter <- colMeans(y)
	    y <- y - rep(ycenter, rep.int(nr, ncy))
	}
	else ycenter <- rep.int(0, ncy)
    }
    else {
	ycenter <- rep_len(ycenter, ncy)
	y <- y - rep(ycenter, rep.int(nr, ncy))
    }
    qx <- qr(x)
    qy <- qr(y)
    dx <- qx$rank;	if(!dx) stop("'x' has rank 0")
    dy <- qy$rank;	if(!dy) stop("'y' has rank 0")
    ## compute svd(Qx'Qy)
    z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx,, drop = FALSE],
             dx, dy)
    xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u)
    rownames(xcoef) <- colnames(x)[qx$pivot][1L:dx]
    ycoef <-  backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v)
    rownames(ycoef) <- colnames(y)[qy$pivot][1L:dy]
    list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter,
	 ycenter = ycenter)
}
