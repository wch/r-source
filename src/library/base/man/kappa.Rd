% file kappa.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{kappa}
\title{Estimate the Condition Number of a Matrix, QR Decomposition or Fit}
\usage{
kappa(z, ...)
kappa.lm(z, ...)
kappa.default(z, exact = FALSE)
}
\alias{kappa}
\alias{kappa.lm}
\alias{kappa.default}
\arguments{
 \item{z}{A matrix or a \code{qr} decomposition or a fit from a class
   inheriting from \code{"lm"}.}
 \item{exact}{Should the result be exact?}
}
\description{
 An estimate of the condition number of a matrix or of the R matrix of a
 QR decomposition, perhaps of a linear fit. The condition number is
 defined as the ratio of the largest to the smallest \emph{non-zero}
 singular value of the matrix.
}
\details{
 If \code{exact = FALSE} (the default) the condition number is estimated
 by a cheap approximation. Following S, this uses the LINPACK routine
 \code{dtrco.f}.  However, in \R (or S) the exact calculation is also
 likely to be quick enough.
}
\value{
  An estimate of the condition number.
}
\author{B.D. Ripley}

\keyword{math}
