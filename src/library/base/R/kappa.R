#  File src/library/base/R/kappa.R
#  Part of the R package, http://www.R-project.org
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

kappa <- function(z, ...) UseMethod("kappa")


### FIXMEs :

### 1)  The condition number is defined WRT to a *norm* !!

###     for exact=TRUE we use the "2-Norm"

### 2)  The  m |--> QR(m)  way is *not* equivalent to using Lapack's *gecon()

## ===> use new argument  'norm = c("2","1","I{nf}")' ,
##       and/or   'method = c("qr","direct")

## Note that  all 4 Lapack version now work in the following
.rcond <- function(x, norm = c("1","I","O"), triangular = FALSE) {
    norm <- match.arg(norm)
    stopifnot(is.matrix(x))
    if(is.complex(x)) {
        if(triangular)
            .Call("La_ztrcon", x, norm, PACKAGE="base")
        else .Call("La_zgecon", x, norm, PACKAGE="base")
    }
    else {
        storage.mode(x) <- "double"
        if(triangular)
            .Call("La_dtrcon", x, norm, PACKAGE="base")
        else .Call("La_dgecon", x, norm, PACKAGE="base")
    }
}

## Further note that  dtrcon ("1") differs quite a bit
## from Linpack's dtrco, which also says to compute the
## 1-norm reciprocal condition

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
    if(exact) kappa.default(z, exact = TRUE)
    else {
	p <- nrow(z)
	if(p != ncol(z)) stop("triangular matrix should be square")
### FIXME: use Lapack's "ztrcon" when z is complex

        ## Note: Linpack's  dtrco  *differs*  from Lapack's dtrcon() !!
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
