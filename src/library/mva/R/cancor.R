## Seber pages 506-507, after a Golub original

cancor <- function(x, y, xcenter=TRUE, ycenter=TRUE)
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if((nr <- nrow(x)) != nrow(y)) stop("unequal number of rows in cancor")
    ncx <- ncol(x)
    ncy <- ncol(y)
    if(!nr || !ncx || !ncy) stop("dimension 0 in x or y")
    if(is.logical(xcenter)) {
	if(xcenter) {
	    xcenter <- apply(x, 2, mean)
	    x <- x - rep(xcenter, rep(nr, ncx))
	}
	else xcenter <- rep(0, ncx)
    }
    else {
	xcenter <- rep(xcenter, length = ncx)
	x <- x - rep(xcenter, rep(nr, ncx))
    }
    if(is.logical(ycenter)) {
	if(ycenter) {
	    ycenter <- apply(y, 2, mean)
	    y <- y - rep(ycenter, rep(nr, ncy))
	}
	else ycenter <- rep(0, ncy)
    }
    else {
	ycenter <- rep(ycenter, length = ncy)
	y <- y - rep(ycenter, rep(nr,ncy))
    }
    qx <- qr(x)
    qy <- qr(y)
    dx <- qx$rank;	if(!dx) stop("`x' has rank 0")
    dy <- qy$rank;	if(!dy) stop("`y' has rank 0")
    ## compute svd(Qx'Qy)
    z <- La.svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
                dx, dy)
    xcoef <- backsolve((qx$qr)[1:dx, 1:dx, drop = FALSE], z$u)
    rownames(xcoef) <- colnames(x)[qx$pivot][1:dx]
    ycoef <-  backsolve((qy$qr)[1:dy, 1:dy, drop = FALSE], t(z$vt))
    rownames(ycoef) <- colnames(y)[qy$pivot][1:dy]
    list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter,
	 ycenter = ycenter)
}
