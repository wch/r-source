kappa <- function(z, ...) UseMethod("kappa")

kappa.lm <- function(z, ...)
{
    kappa.qr(z$qr, ...)
}

kappa.default <- function(z, exact = FALSE, ...)
{
    z <- as.matrix(z)
    if(exact) {
	s <- svd(z, nu=0, nv=0)$d
	max(s)/min(s[s > 0])
    } else if(is.qr(z)) kappa.qr(z)
    else if(nrow(z) < ncol(z)) kappa.qr(qr(t(z)))
    else kappa.qr(qr(z))
}

kappa.qr <- function(z, ...)
{
    qr <- z$qr
    R <- qr[1:min(dim(qr)), , drop = FALSE]
    R[lower.tri(R)] <- 0
    kappa.tri(R, ...)
}

kappa.tri <- function(z, exact = FALSE, ...)
{
    if(exact) kappa.default(z)
    else {
	p <- nrow(z)
	if(p != ncol(z)) stop("matrix should be square")
	1 / .Fortran("dtrco",
		     as.double(z),
		     p,
		     p,
		     k = double(1),
		     double(p),
		     as.integer(1),
                     PACKAGE="base")$k
    }
}
