% file kappa.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{kappa}
\title{Estimate the Condition Number}
\usage{
kappa(z, \dots)
kappa.lm     (z, \dots)
kappa.default(z, exact = FALSE)
kappa.qr     (z, \dots) 
kappa.tri    (z, exact = FALSE, \dots)
}
\alias{kappa}
\alias{kappa.default}
\alias{kappa.lm}
\alias{kappa.qr}
\alias{kappa.tri}
\arguments{
 \item{z}{A matrix or a the result of \code{\link{qr}} or a fit from a class
   inheriting from \code{"lm"}.}
 \item{exact}{Should the result be exact?}
}
\description{
 An estimate of the condition number of a matrix or of the \eqn{R} matrix of a
 \eqn{QR} decomposition, perhaps of a linear fit.  The condition number is
 defined as the ratio of the largest to the smallest \emph{non-zero}
 singular value of the matrix.
}
\details{
 If \code{exact = FALSE} (the default) the condition number is estimated
 by a cheap approximation. Following S, this uses the LINPACK routine
 \file{dtrco.f}.  However, in \R (or S) the exact calculation is also
 likely to be quick enough.
}
\value{
    The condition number, \eqn{kappa}, or an approximation if
    \code{exact=FALSE}.  
}
\author{B.D. Ripley}
\seealso{\code{\link{svd}} for the singular value decomposition and
        \code{\link{qr}} for the \eqn{QR} one.
}
\examples{
kappa(x1 <- cbind(1,1:10))# 15.71
kappa(x1, exact=T)        # 13.68
kappa(x2 <- cbind(x1,2:11))# high! [x2 is singular!]

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
sv9 <- svd(h9 <- hilbert(9))$ d
kappa(h9)# pretty high!
kappa(h9, exact=TRUE) == max(sv9) / min(sv9)
kappa(h9, exact=TRUE) / kappa(h9) # .677 (i.e. rel.error = 32\%)
}
\keyword{math}
