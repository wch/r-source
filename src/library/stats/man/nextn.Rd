% File src/library/stats/man/nextn.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{nextn}
\title{Find Highly Composite Numbers}
\usage{
nextn(n, factors = c(2,3,5))
}
\alias{nextn}
\arguments{
  \item{n}{a vector of integer numbers (of type \code{"integer"}
    \emph{or} \code{"double"}).}
  \item{factors}{a vector of positive integer factors (at least \eqn{2}
    and preferably relative prime, see the note).}
}
\description{
  \code{nextn} returns the smallest integer,
  greater than or equal to \code{n}, which can be obtained
  as a product of powers of the values contained in \code{factors}.

  \code{nextn()} is intended to be used to find a suitable length
  to zero-pad the argument of \code{\link{fft}}
  so that the transform is computed quickly.
  The default value for \code{factors} ensures this.
}
\note{If the factors in \code{factors} are \emph{not} relative prime,
  i.e., have themselves a common factor larger than one, the result may
  be wrong in the sense that it may not be the \emph{smallest} integer.
  E.g., \code{nextn(91, c(2,6))} returns 128 instead of 96 as
  \code{nextn(91, c(2,3))} returns.
}
\seealso{
  \code{\link{convolve}}, \code{\link{fft}}.
}
\examples{
nextn(1001) # 1024
table(nextn(599:630))
n <- 1:100 ; plot(n, nextn(n) - n, type = "o", lwd=2, cex=1/2)
}
\keyword{math}

