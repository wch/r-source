% File src/library/stats/man/diffinv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{diffinv}
\alias{diffinv}
\alias{diffinv.default}
\alias{diffinv.ts}
\title{Discrete Integration: Inverse of Differencing}
\description{
  Computes the inverse function of the lagged differences function
  \code{\link{diff}}.
}
\usage{
diffinv(x, ...)

\method{diffinv}{default}(x, lag = 1, differences = 1, xi, \dots)
\method{diffinv}{ts}(x, lag = 1, differences = 1, xi, \dots)
}
\arguments{
  \item{x}{a numeric vector, matrix, or time series.}
  \item{lag}{a scalar lag parameter.}
  \item{differences}{an integer representing the order of the
    difference.}
  \item{xi}{a numeric vector, matrix, or time series containing the
    initial values for the integrals.  If missing, zeros are used.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  \code{diffinv} is a generic function with methods for class \code{"ts"}
  and \code{default} for vectors and matrices.

  Missing values are not handled.
}
\value{
  A numeric vector, matrix, or time series (the latter for the
  \code{"ts"} method) representing the discrete integral of \code{x}.
}
\author{A. Trapletti}
\seealso{
  \code{\link{diff}}
}
\examples{
s <- 1:10
d <- diff(s)
diffinv(d, xi = 1)
}
\keyword{ts}
