\name{df.residual}
\title{Residual Degrees-of-Freedom}
\usage{
df.residual(x, ...)
}
\alias{df.residual}
\arguments{
\item{x}{an object for which the degrees-of-freedom are desired.}
\item{...}{additional optional arguments.}
}
\value{
The value of the residual degrees-of-freedom
extracted from the object \code{x}.

This is a generic function which can be used to extract
residual degrees-of-freedom for fitted models.
Consult the individual modeling functions for details
details on how to use this function

There is no default method for this function.
}
\seealso{
\code{\link{deviance}}, \code{\link{glm}}, \code{\link{lm}}.
}
\keyword{models}
\keyword{regression}
