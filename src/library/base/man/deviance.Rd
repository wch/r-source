\name{deviance}
\alias{deviance}
\alias{deviance.default}
\alias{deviance.lm}
\alias{deviance.glm}
\alias{deviance.mlm}
\title{Model Deviance}
\description{
  Returns the deviance of a fitted model object.
}
\usage{
deviance(object, \dots)
deviance.lm (object, \dots)
deviance.glm(object, \dots)
deviance.mlm(object, \dots)
deviance.default(object, \dots)
}
\arguments{
  \item{object}{an object for which the deviance is desired.}
  \item{\dots}{additional optional argument.}
}
\details{
  This is a generic function which can be used to extract deviances for
  fitted models.  Consult the individual modeling functions for details
  details on how to use this function.

  There is no default method for this function.
}
\value{
  The value of the deviance extracted from the object \code{object}.
}
\seealso{
  \code{\link{df.residual}},
  \code{\link{extractAIC}},
  \code{\link{glm}},
  \code{\link{lm}}.
}
\keyword{models}
