\name{ts-internal}
\alias{.cbind.ts}
\alias{arma0f}
\title{Internal ts functions}
\description{
  Internal ts functions.
}
\usage{
arma0f(p)
.cbind.ts(\dots, dframe = FALSE, union = TRUE)
}
\arguments{
  \item{\dots}{two or more univariate or multivariate time series, or
    objects which can coerced to time series.}
  \item{dframe}{logical; if \code{TRUE} return the result as a data
    frame.}
  \item{union}{logical; if \code{TRUE}, act as \code{ts.union} or
    \code{ts.intersect}.}
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
