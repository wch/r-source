\name{deviance}
\alias{deviance}
%\alias{deviance.default}
%\alias{deviance.lm}
%\alias{deviance.glm}
%\alias{deviance.mlm}
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
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{df.residual}},
  \code{\link{extractAIC}},
  \code{\link{glm}},
  \code{\link{lm}}.
}
\keyword{models}
