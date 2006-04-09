\name{df.residual}
\alias{df.residual}
%\alias{df.residual.default}
\title{Residual Degrees-of-Freedom}
\description{
  Returns the residual degrees-of-freedom extracted from a fitted model
  object.
}
\usage{
df.residual(object, \dots)
}
\arguments{
  \item{object}{an object for which the degrees-of-freedom are desired.}
  \item{\dots}{additional optional arguments.}
}
\details{
  This is a generic function which can be used to extract residual
  degrees-of-freedom for fitted models.  Consult the individual modeling
  functions for details on how to use this function.

  The default method just extracts the \code{df.residual} component.
}
\value{
  The value of the residual degrees-of-freedom extracted from the object
  \code{x}.
}
\seealso{
  \code{\link{deviance}}, \code{\link{glm}}, \code{\link{lm}}.
}
\keyword{models}
\keyword{regression}
