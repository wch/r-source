% File src/library/stats/man/fitted.values.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{weights}
\alias{weights}
\alias{weights.default}

\title{Extract Model Weights}
\usage{
weights(object, \dots)
}
\arguments{
  \item{object}{an object for which the extraction of model weights is
    meaningful.}
  \item{\dots}{other arguments passed to methods.}
}
\description{
  \code{weights} is a generic function which extracts fitting weights from
  objects returned by modeling functions.

  Methods can make use of \code{\link{napredict}} methods to compensate
  for the omission of missing values.  The default methods does so.
}
\value{
  Weights extracted from the object \code{object}: the default method
  looks for component \code{"weights"} and if not \code{NULL} calls
  \code{\link{napredict}} on it.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{weights.glm}}
}
\keyword{models}
