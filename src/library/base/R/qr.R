#is.qr <- function(x) !is.null(x$qr) && !is.null(x$rank) && !is.null(x$qraux)

is.qr <- function(x) inherits(x, "qr")

qr <- function(x, tol = 1e-07, LAPACK = FALSE)
{
    x <- as.matrix(x)
    if(is.complex(x))
        return(structure(.Call("La_zgeqp3", x, PACKAGE = "base"), class="qr"))
    if(LAPACK) {
        res <- .Call("La_dgeqp3", x, PACKAGE = "base")
        attr(res, "useLAPACK") <- TRUE
        class(res) <- "qr"
        return(res)
    }

    p <- ncol(x) # guaranteed to be integer
    n <- nrow(x)
    if(!is.double(x))
	storage.mode(x) <- "double"
    res <- .Fortran("dqrdc2",
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
    class(res) <- "qr"
    res
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
    ny <- ncol(y)
    if (p==0) return( if (im) matrix(0,p,ny) else numeric(0) )
    if(is.complex(qr$qr)) {
	if(!is.complex(y)) y[] <- as.complex(y)
	coef <- matrix(as.complex(NA), nr=p, nc=ny)
	coef[qr$pivot,] <- .Call("qr_coef_cmplx", qr, y, PACKAGE = "base")[1:p]
	return(if(im) coef else c(coef))
    }
    ## else {not complex} :
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a) {
	coef <- matrix(as.double(NA), nr=p, nc=ny)
	coef[qr$pivot,] <- .Call("qr_coef_real", qr, y, PACKAGE = "base")[1:p]
	return(if(im) coef else c(coef))
    }
    if (k==0) return( if (im) matrix(NA,p,ny) else rep.int(NA,p))

    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    z <- .Fortran("dqrcf",
		  as.double(qr$qr),
		  n, k,
		  as.double(qr$qraux),
		  y,
		  ny,
		  coef=matrix(0,nr=k,nc=ny),
		  info=integer(1),
		  NAOK = TRUE, PACKAGE="base")[c("coef","info")]
    if(z$info != 0) stop("exact singularity in 'qr.coef'")
    if(k < p) {
	coef <- matrix(as.double(NA), nr=p, nc=ny)
	coef[qr$pivot[1:k],] <- z$coef
    }
    else coef <- z$coef

    if(!is.null(nam <- colnames(qr$qr)))
	rownames(coef) <- nam
    if(im && !is.null(nam <- colnames(y)))
       colnames(coef) <- nam

    if(im) coef else drop(coef)
}

qr.qy <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) {
        y <- as.matrix(y)
        if(!is.complex(y)) y[] <- as.complex(y)
        return(.Call("qr_qy_cmplx", qr, y, 0, PACKAGE = "base"))
    }
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        return(.Call("qr_qy_real", qr, as.matrix(y), 0, PACKAGE = "base"))
    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    ny <- NCOL(y)
    storage.mode(y) <- "double"
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    .Fortran("dqrqy",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qy = y,# incl. {dim}names
	     PACKAGE="base")$qy
}

qr.qty <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)){
        y <- as.matrix(y)
        if(!is.complex(y)) y[] <- as.complex(y)
        return(.Call("qr_qy_cmplx", qr, y, 1, PACKAGE = "base"))
    }
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        return(.Call("qr_qy_real", qr, as.matrix(y), 1, PACKAGE = "base"))

    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    k <- as.integer(qr$rank)
    ny <- NCOL(y)
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrqty",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qty = y,# incl. {dim}names
             PACKAGE = "base")$qty
}

qr.resid <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        stop("not supported for LAPACK QR")
    k <- as.integer(qr$rank)
    if (k==0) return(y)
    n <- nrow(qr$qr)
#    p <- ncol(qr$qr)
    ny <- NCOL(y)
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrrsd",
	     as.double(qr$qr),	     n, k,
	     as.double(qr$qraux),
             y,
	     ny,
	     rsd = y,# incl. {dim}names
	     PACKAGE="base")$rsd
}

qr.fitted <- function(qr, y, k=qr$rank)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        stop("not supported for LAPACK QR")
    n <- nrow(qr$qr)
    k <- as.integer(k)
    if(k > qr$rank) stop("'k' is too large")
    ny <- NCOL(y)
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrxb",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     xb = (yy <- y),# incl. {dim}names
             DUP=FALSE, PACKAGE="base")$xb
}

## qr.solve is defined in  ./solve.R

##---- The next three are from Doug Bates ('st849'):
qr.Q <- function (qr, complete = FALSE,
		  Dvec = rep.int(if (cmplx) 1 + 0i else 1,
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
    pivoted <- !identical(qr$pivot, seq(along=qr$pivot))
    R <- qr.R(qr, complete = TRUE)
    if(pivoted && ncol < length(qr$pivot))
        stop("need larger value of 'ncol' as pivoting occurred")
    cmplx <- mode(R) == "complex"
    p <- dim(R)[2]
    if (ncol < p)
	R <- R[, 1:ncol, drop = FALSE]
    else if (ncol > p) {
	tmp <- diag(if (!cmplx) 1 else 1 + 0i, nrow(R), ncol)
	tmp[, 1:p] <- R
	R <- tmp
    }
    res <- qr.qy(qr, R)
    if(pivoted) res[, qr$pivot] <- res[, seq(along=qr$pivot)]
    res
}
