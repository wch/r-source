\name{mean}
\title{Arithmetic Mean}
\usage{
mean(x, \dots)
mean.default(x, trim = 0, na.rm = FALSE)
}
\alias{mean}
\alias{mean.default}
\arguments{
  \item{x}{a numeric vector containing the values
    whose mean is to be computed.  A complex vector is allowed for
    \code{trim=0}, only.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
}
\description{
  Generic function for the (trimmed) arithmetic mean.
}
\value{
  If \code{trim} is zero (the default),
  the arithmetic mean of the values in \code{x} is computed.

  If \code{trim} is non-zero, a symmetrically trimmed mean is computed
  with a fraction of \code{trim} observations deleted from each end
  before the mean is computed.
}
\seealso{
  \code{\link{weighted.mean}}
}
\examples{
x <- c(0:10, 50)
xm <- mean(x)
c(xm, mean(x, trim = .10))

all.equal(mean(x, trim = 0.5), median(x))
}
\keyword{univar}

