\name{diffinv}
\alias{diffinv}
\alias{diffinv.ts}
\alias{diffinv.default}
\alias{diffinv.vec}
\title{Discrete Integrals: Inverse of Differencing}
\usage{
diffinv(x, lag = 1, differences = 1,
    xi = rep(0.0, lag*differences*NCOL(x)))
}
\arguments{
  \item{x}{a numeric vector, matrix, or time series.}
  \item{lag}{a scalar lag parameter.}
  \item{differences}{an integer representing the order of the difference.}
  \item{xi}{a numeric vector, matrix, or time series containing the
    initial values for the integrals.}
}
\description{
  Computes the inverse function of the lagged differences
  \code{\link{diff}}. 

  Missing values are not handled.
}
\details{
  \code{diffinv} is a generic function with methods for class \code{"ts"}
  and \code{default} for vectors and matrices.
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
diffinv(d,xi=1)
}
\keyword{ts}

