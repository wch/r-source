\name{diffinv}
\alias{diffinv}
\alias{diffinv.default}
\alias{diffinv.ts}
\alias{diffinv.vector}
\title{Discrete Integration: Inverse of Differencing}
\description{
  Computes the inverse function of the lagged differences function
  \code{\link{diff}}. 
}
\synopsis{
diffinv(x, ...)
diffinv.default(x, lag = 1, differences = 1,
    xi = rep(0, lag * differences * dim(as.matrix(x))[2]), ...) 
diffinv.ts(x, lag = 1, differences = 1,
    xi = rep(0, lag * differences * NCOL(x)), ...) 
diffinv.vector(x, lag = 1, differences = 1,
    xi = rep(0, lag * differences), ...)
}
\usage{
diffinv(x, lag = 1, differences = 1,
        xi = rep(0.0, lag*differences*NCOL(x)), ...)
}
\arguments{
  \item{x}{a numeric vector, matrix, or time series.}
  \item{lag}{a scalar lag parameter.}
  \item{differences}{an integer representing the order of the
    difference.}
  \item{xi}{a numeric vector, matrix, or time series containing the
    initial values for the integrals.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  \code{diffinv} is a generic function with methods for class \code{"ts"}
  and \code{default} for vectors and matrices.

  Missing values are not handled.
}
\value{
  A numeric vector, matrix, or time series representing the discrete
  integral of \code{x}. 
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
