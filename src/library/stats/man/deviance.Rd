% File src/library/stats/man/deviance.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{deviance}
\alias{deviance}
\title{Model Deviance}
\description{
  Returns the deviance of a fitted model object.
}
\usage{
deviance(object, \dots)
}
\arguments{
  \item{object}{an object for which the deviance is desired.}
  \item{\dots}{additional optional argument.}
}
\details{
  This is a generic function which can be used to extract deviances for
  fitted models.  Consult the individual modeling functions for details
  on how to use this function.
}
\value{
  The value of the deviance extracted from the object \code{object}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{df.residual}},
  \code{\link{extractAIC}},
  \code{\link{glm}},
  \code{\link{lm}}.
}
\keyword{models}
