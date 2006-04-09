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
	xcenter <- rep(xcenter, length.out = ncx)
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
	ycenter <- rep(ycenter, length.out = ncy)
	y <- y - rep(ycenter, rep.int(nr,ncy))
    }
    qx <- qr(x)
    qy <- qr(y)
    dx <- qx$rank;	if(!dx) stop("'x' has rank 0")
    dy <- qy$rank;	if(!dy) stop("'y' has rank 0")
    ## compute svd(Qx'Qy)
    z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
             dx, dy)
    xcoef <- backsolve((qx$qr)[1:dx, 1:dx, drop = FALSE], z$u)
    rownames(xcoef) <- colnames(x)[qx$pivot][1:dx]
    ycoef <-  backsolve((qy$qr)[1:dy, 1:dy, drop = FALSE], z$v)
    rownames(ycoef) <- colnames(y)[qy$pivot][1:dy]
    list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter,
	 ycenter = ycenter)
}
