% File src/library/stats/man/tsp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

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
  The \code{tsp} attribute gives the start time \emph{in time units},
  the end time and the frequency (the number of observations per unit of
  time, e.g.\sspace{}12 for a monthly series).

  Assignments are checked for consistency.

  Assigning \code{NULL} which removes the \code{tsp} attribute
  \emph{and} any \code{"ts"} (or \code{"mts"}) class of \code{x}.
}
\value{
  An object which differs from \code{x} only in the \code{tsp} attribute
  (unless \code{NULL} is assigned).

  \code{hasTsp} adds, if needed, an attribute with a start time and
  frequency of 1 and end time \code{\link{NROW}(x)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{ts}}, \code{\link{time}}, \code{\link{start}}.
}
\keyword{ts}
