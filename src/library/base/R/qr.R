is.qr <- function(x) !is.null(x$qr) && !is.null(x$rank) && !is.null(x$qraux)

qr <- function(x, tol= 1e-07)
{
    x <- as.matrix(x)
    p <- as.integer(ncol(x))
    n <- as.integer(nrow(x))
    if(!is.double(x))
	storage.mode(x) <- "double"
    .Fortran("dqrdc2",
	     qr=x,
	     n,
	     n,
	     p,
	     as.double(tol),
	     rank=integer(1),
	     qraux = double(p),
	     pivot = as.integer(1:p),
	     double(2*p),
	     PACKAGE="base")[c(1,6,7,8)]# c("qr", "rank", "qraux", "pivot")
}

qr.coef <- function(qr, y)
{
    if( !is.qr(qr) )
	stop("first argument must be a QR decomposition")
    n <- nrow(qr$qr)
    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    im <- is.matrix(y)
    if (!im) y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    if (p==0) return( if (im) matrix(0,p,ny) else numeric(0) )
    if (k==0) return( if (im) matrix(NA,p,ny) else rep(NA,p))
    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("qr and y must have the same number of rows")
    z <- .Fortran("dqrcf",
		  as.double(qr$qr),
		  n, k,
		  as.double(qr$qraux),
		  y,
		  ny,
		  coef=matrix(0,nr=k,nc=ny),
		  info=integer(1),
		  NAOK = TRUE, PACKAGE="base")[c("coef","info")]
    if(z$info != 0) stop("exact singularity in qr.coef")
    if(k < p) {
	coef <- matrix(as.double(NA),nr=p,nc=ny)
	coef[qr$pivot[1:k],] <- z$coef
    }
    else coef <- z$coef

    if(im) coef else c(coef)
}

qr.qy <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    n <- as.integer(nrow(qr$qr))
    p <- as.integer(ncol(qr$qr))
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    storage.mode(y) <- "double"
    if(NROW(y) != n)
	stop("qr and y must have the same number of rows")
    qy <- if(is.matrix(y)) matrix(double(n*ny), n, ny) else double(n)
    .Fortran("dqrqy",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qy=qy,
	     PACKAGE="base")$qy
}

qr.qty <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    n <- as.integer(nrow(qr$qr))
    p <- as.integer(ncol(qr$qr))
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    storage.mode(y) <- "double"
    if(NROW(y) != n)
	stop("qr and y must have the same number of rows")
    qty <- if(is.matrix(y)) matrix(double(n*ny), n, ny) else double(n)
    .Fortran("dqrqty",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qty=qty,
             PACKAGE = "base")$qty
}

qr.resid <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    k <- as.integer(qr$rank)
    if (k==0) return(y)
    n <- as.integer(nrow(qr$qr))
    p <- as.integer(ncol(qr$qr))
    y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("qr and y must have the same number of rows")
    .Fortran("dqrrsd",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     rsd=mat.or.vec(n,ny),
	     PACKAGE="base")$rsd
}

qr.fitted <- function(qr, y, k=qr$rank)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    n <- as.integer(nrow(qr$qr))
    p <- as.integer(ncol(qr$qr))
    k <- as.integer(k)
    if(k > qr$rank) stop("k is too large")
    y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("qr and y must have the same number of rows")
    .Fortran("dqrxb",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     xb=mat.or.vec(n,ny), DUP=FALSE, PACKAGE="base")$xb
}

## qr.solve is defined in  ./solve.R

##---- The next three are from Doug Bates ('st849'):
qr.Q <- function (qr, complete = FALSE,
		  Dvec = rep(if (cmplx) 1 + 0i else 1,
		  if (complete) dqr[1] else min(dqr)))
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    dqr <- dim(qr$qr)
    cmplx <- mode(qr$qr) == "complex"
    D <-
	if (complete) diag(Dvec, dqr[1])
	else {
	    ncols <- min(dqr)
	    diag(Dvec[1:ncols], nrow = dqr[1], ncol = ncols)
	}
    qr.qy(qr, D)
}

qr.R <- function (qr, complete = FALSE)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    R <- qr$qr
    if (!complete)
	R <- R[seq(min(dim(R))), , drop = FALSE]
    R[row(R) > col(R)] <- 0
    R
}

qr.X <- function (qr, complete = FALSE,
		  ncol = if (complete) nrow(R) else min(dim(R)))
{

    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    R <- qr.R(qr, complete = TRUE)
    cmplx <- mode(R) == "complex"
    p <- dim(R)[2]
    if (ncol < p)
	R <- R[, 1:ncol, drop = FALSE]
    else if (ncol > p) {
	tmp <- diag(if (!cmplx) 1 else 1 + 0i, nrow(R), ncol)
	tmp[, 1:p] <- R
	R <- tmp
    }
    qr.qy(qr, R)
}
