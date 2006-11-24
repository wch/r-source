\name{tsp}
\title{Tsp Attribute of Time-Series-like Objects}
\alias{tsp}
\alias{tsp<-}
\alias{hasTsp}
\usage{
tsp(x)
tsp(x) <- value
hasTsp(x)
}
\arguments{
    \item{x}{a vector or matrix or univariate or multivariate time-series.}
    \item{value}{a numeric vector of length 3 or \code{NULL}.}
}
\description{
    \code{tsp} returns the \code{tsp} attribute (or \code{NULL}).
    It is included for compatibility with S version 2. \code{tsp<-}
    sets the \code{tsp} attribute. \code{hasTsp} ensures \code{x} has a
    \code{tsp} attribute, by adding one if needed.
}
\details{
    The \code{tsp} attribute was previously described here as
    \code{c(start(x), end(x), frequency(x))}, but this is incorrect.
    It gives the start time \emph{in time units}, the end time and
    the frequency.

    Assignments are checked for consistency.    

    Assigning \code{NULL} which removes the \code{tsp} attribute
    \emph{and} any \code{"ts"} (or \code{"mts"}) class of \code{x}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
    \code{\link{ts}}, \code{\link{time}}, \code{\link{start}}.
}
\keyword{ts}
