% File src/library/base/man/mean.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{mean}
\title{Arithmetic Mean}
\usage{
mean(x, \dots)

\method{mean}{default}(x, trim = 0, na.rm = FALSE, \dots)
}
\alias{mean}
\alias{mean.default}
\arguments{
  \item{x}{An \R object.  Currently there are methods for
    numeric/logical vectors and \link[=Dates]{date},
    \link{date-time} and \link{time interval} objects.  Complex vectors
    are allowed for \code{trim = 0}, only.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.
    Values of trim outside that range are taken as the nearest endpoint.
  }
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Generic function for the (trimmed) arithmetic mean.
}
\value{
  If \code{trim} is zero (the default), the arithmetic mean of the
  values in \code{x} is computed, as a numeric or complex vector of
  length one.  If \code{x} is not logical (coerced to numeric), numeric
  (including integer) or complex, \code{NA_real_} is returned, with a warning.

  If \code{trim} is non-zero, a symmetrically trimmed mean is computed
  with a fraction of \code{trim} observations deleted from each end
  before the mean is computed.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{weighted.mean}}, \code{\link{mean.POSIXct}},
  \code{\link{colMeans}} for row and column means.
}
\examples{
x <- c(0:10, 50)
xm <- mean(x)
c(xm, mean(x, trim = 0.10))
}
\keyword{univar}

