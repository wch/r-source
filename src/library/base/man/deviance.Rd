\name{deviance}
\title{Model Deviance}
\usage{
deviance(x, ...)

deviance.lm (x, ...)
deviance.glm(x, ...)
deviance.mlm(x, ...)
deviance.default(x, ...)
}
\alias{deviance}
\alias{deviance.default}
\alias{deviance.lm}
\alias{deviance.glm}
\alias{deviance.mlm}
\arguments{
\item{x}{an object for which the deviance is desired.}
\item{...}{additional optional argument.}
}
\value{
The value of the deviance extracted from the object \code{x}.

This is a generic function which can be used to extract
deviances for fitted models.
Consult the individual modeling functions for details
details on how to use this function

There is no default method for this function.
}
\seealso{
\code{\link{df.residual}}, \code{\link{extractAIC}}, \code{\link{glm}}, \code{\link{lm}}.
}
\keyword{models}
