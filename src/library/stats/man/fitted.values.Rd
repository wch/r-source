% File src/library/stats/man/fitted.values.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{fitted}
\title{Extract Model Fitted Values}
\usage{
fitted(object, \dots)
fitted.values(object, \dots)
}
\alias{fitted.values}
\alias{fitted}
\alias{fitted.default}
\arguments{
  \item{object}{an object for which the extraction of model fitted values is
    meaningful.}
  \item{\dots}{other arguments.}
}
\description{
  \code{fitted} is a generic function which extracts fitted values from
  objects returned by modeling functions.  \code{fitted.values} is an
  alias for it.

  All object classes which are returned by model fitting functions
  should provide a \code{fitted} method.  (Note that the generic is
  \code{fitted} and not \code{fitted.values}.)

  Methods can make use of \code{\link{napredict}} methods to compensate
  for the omission of missing values.  The default and \code{\link{nls}}
  methods do.
}
\value{
  Fitted values extracted from the object \code{object}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{coefficients}}, \code{\link{glm}}, \code{\link{lm}},
  \code{\link{residuals}}.
}
\keyword{models}
\keyword{regression}
