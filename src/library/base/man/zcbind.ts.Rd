\name{zcbind}
\alias{.cbind.ts}
\title{Bind Two or More Time Series}
\description{
  Bind Two or More Time Series which have common frequency.
}
\usage{
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
  This is an internal function which is not to be called by the user.
}
\keyword{internal}
