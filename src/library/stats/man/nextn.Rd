% File src/library/stats/man/nextn.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{nextn}
\title{Highly Composite Numbers}
\usage{
nextn(n, factors = c(2,3,5))
}
\alias{nextn}
\arguments{
\item{n}{an integer.}
\item{factors}{a vector of positive integer factors.}
}
\description{
  \code{nextn} returns the smallest integer,
  greater than or equal to \code{n}, which can be obtained
  as a product of powers of the values contained in \code{factors}.
  \code{nextn} is intended to be used to find a suitable length
  to zero-pad the argument of \code{fft} to
  so that the transform is computed quickly.
  The default value for \code{factors} ensures this.
}
\seealso{
\code{\link{convolve}}, \code{\link{fft}}.
}
\examples{
nextn(1001) # 1024
table(sapply(599:630, nextn))
}
\keyword{math}

