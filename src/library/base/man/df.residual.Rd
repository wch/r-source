\name{df.residual}
\alias{df.residual}
\title{Residual Degrees-of-Freedom}
\description{
  Returns the residual degrees-of-freedom extracted from a fitted model
  object.
\usage{
df.residual(x, \dots)
}
\arguments{
  \item{x}{an object for which the degrees-of-freedom are desired.}
  \item{\dots}{additional optional arguments.}
}
\details{
  This is a generic function which can be used to extract residual
  degrees-of-freedom for fitted models.  Consult the individual modeling
  functions for details details on how to use this function.

  There is no default method for this function.
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
