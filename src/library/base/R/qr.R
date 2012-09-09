#  File src/library/base/R/qr.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

#is.qr <- function(x) !is.null(x$qr) && !is.null(x$rank) && !is.null(x$qraux)

is.qr <- function(x) inherits(x, "qr")

qr <- function(x, ...) UseMethod("qr")

qr.default <- function(x, tol = 1e-07, LAPACK = FALSE, ...)
{
    x <- as.matrix(x)
    if(is.complex(x)) {
        res <- .Call("La_zgeqp3", x, PACKAGE = "base")
         if(!is.null(cn <- colnames(x))) colnames(res$qr) <- cn[res$pivot]
       return(structure(res, class = "qr"))
    }
    ## otherwise :
    if(!is.double(x))
	storage.mode(x) <- "double"
    if(LAPACK) {
        res <- .Call("La_dgeqp3", x, PACKAGE = "base")
        if(!is.null(cn <- colnames(x))) colnames(res$qr) <- cn[res$pivot]
        attr(res, "useLAPACK") <- TRUE
        class(res) <- "qr"
        return(res)
    }

    p <- as.integer(ncol(x))
    if(is.na(p)) stop("invalid ncol(x)")
    n <- as.integer(nrow(x))
    if(is.na(n)) stop("invalid nrow(x)")
    res <- .Fortran("dqrdc2",
	     qr = x,
	     n,
	     n,
	     p,
	     as.double(tol),
	     rank = integer(1L),
	     qraux = double(p),
	     pivot = as.integer(1L:p),
	     double(2*p),
	     PACKAGE = "base")[c(1,6,7,8)]# c("qr", "rank", "qraux", "pivot")
    if(!is.null(cn <- colnames(x)))
        colnames(res$qr) <- cn[res$pivot]
    class(res) <- "qr"
    res
}

## + qr.lm  method defined in ../../stats/R/lm.R


qr.coef <- function(qr, y)
{
    if( !is.qr(qr) )
	stop("first argument must be a QR decomposition")
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop("invalid nrow(qr$qr)")
    p <- as.integer(ncol(qr$qr))
    if(is.na(p)) stop("invalid ncol(qr$qr)")
    k <- as.integer(qr$rank)
    if(is.na(k)) stop("invalid ncol(qr$rank)")
    im <- is.matrix(y)
    if (!im) y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    if(is.na(ny)) stop("invalid ncol(y)")
    if (p == 0L) return( if (im) matrix(0, p, ny) else numeric() )
    ix <- if ( p > n ) c(seq_len(n), rep(NA, p - n)) else seq_len(p)
    if(is.complex(qr$qr)) {
	if(!is.complex(y)) y[] <- as.complex(y)
	coef <- matrix(NA_complex_, nrow = p, ncol = ny)
	coef[qr$pivot,] <-
            .Call("qr_coef_cmplx", qr, y, PACKAGE = "base")[ix, ]
	return(if(im) coef else c(coef))
    }
    ## else {not complex} :
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a) {
        if(!is.double(y)) storage.mode(y) <- "double"
	coef <- matrix(NA_real_, nrow = p, ncol = ny)
	coef[qr$pivot,] <-
            .Call("qr_coef_real", qr, y, PACKAGE = "base")[ix,]
	return(if(im) coef else c(coef))
    }
    if (k == 0L) return( if (im) matrix(NA, p, ny) else rep.int(NA, p))

    storage.mode(y) <- "double"
    if( nrow(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    z <- .Fortran("dqrcf",
		  as.double(qr$qr),
		  n, k,
		  as.double(qr$qraux),
		  y,
		  ny,
		  coef = matrix(0, nrow = k,ncol = ny),
		  info = integer(1L),
		  NAOK = TRUE, PACKAGE = "base")[c("coef","info")]
    if(z$info) stop("exact singularity in 'qr.coef'")
    if(k < p) {
	coef <- matrix(NA_real_, nrow = p, ncol = ny)
	coef[qr$pivot[1L:k],] <- z$coef
    }
    else coef <- z$coef

    if(!is.null(nam <- colnames(qr$qr)))
        if(k < p) rownames(coef)[qr$pivot] <- nam
        else rownames(coef) <- nam

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
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop("invalid nrow(qr$qr)")
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop("invalid NCOL(y)")
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

    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop("invalid nrow(qr$qr)")
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop("invalid NCOL(y)")
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
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop("invalid nrow(qr$qr)")
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop("invalid NCOL(y)")
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrrsd",
	     as.double(qr$qr),	     n, k,
	     as.double(qr$qraux),
             y,
	     ny,
	     rsd = y,# incl. {dim}names
	     PACKAGE = "base")$rsd
}

qr.fitted <- function(qr, y, k=qr$rank)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    a <- attr(qr, "useLAPACK")
    if(!is.null(a) && is.logical(a) && a)
        stop("not supported for LAPACK QR")
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop("invalid nrow(qr$qr)")
    k <- as.integer(k)
    if(k > qr$rank) stop("'k' is too large")
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop("invalid NCOL(y)")
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran("dqrxb",
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     xb = y,# incl. {dim}names
             PACKAGE = "base")$xb
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
	if (complete) diag(Dvec, dqr[1L])
	else {
	    ncols <- min(dqr)
	    diag(Dvec[1L:ncols], nrow = dqr[1L], ncol = ncols)
	}
    qr.qy(qr, D)
}

qr.R <- function (qr, complete = FALSE)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    R <- qr$qr
    if (!complete)
	R <- R[seq.int(min(dim(R))), , drop = FALSE]
    R[row(R) > col(R)] <- 0
    R
}

qr.X <- function (qr, complete = FALSE,
		  ncol = if (complete) nrow(R) else min(dim(R)))
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    pivoted <- !identical(qr$pivot, ip <- seq_along(qr$pivot))
    R <- qr.R(qr, complete = TRUE)
    if(pivoted && ncol < length(qr$pivot))
        stop("need larger value of 'ncol' as pivoting occurred")
    cmplx <- mode(R) == "complex"
    p <- as.integer(dim(R)[2L])
    if(is.na(p)) stop("invalid NCOL(R)")
    if (ncol < p)
	R <- R[, 1L:ncol, drop = FALSE]
    else if (ncol > p) {
	tmp <- diag(if (!cmplx) 1 else 1 + 0i, nrow(R), ncol)
	tmp[, 1L:p] <- R
	R <- tmp
    }
    res <- qr.qy(qr, R)
    cn <- colnames(res)
    if(pivoted) {# res may have more columns than length(qr$pivot)
	res[, qr$pivot] <- res[, ip]
        if(!is.null(cn)) colnames(res)[qr$pivot] <- cn[ip]
    }
    res
}
