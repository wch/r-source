## Seber pages 506-507, after a Golub original

cancor <- function(x, y, xcenter=TRUE, ycenter=TRUE)
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if((nr <- nrow(x)) != nrow(y)) stop("unequal number of rows in cancor")
    ncx <- ncol(x)
    ncy <- ncol(y)
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
    dx <- qx$rank
    dy <- qy$rank
    ## compute svd(Qx'Qy)
    z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
	     dx, dy)
    list(cor = z$d,
	 xcoef = backsolve((qx$qr)[1:dx, 1:dx, drop = FALSE], z$u),
	 ycoef = backsolve((qy$qr)[1:dy, 1:dy, drop = FALSE], z$v),
	 xcenter = xcenter,
	 ycenter = ycenter)
}
