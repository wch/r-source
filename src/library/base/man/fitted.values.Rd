\name{fitted.values}
\title{Extract Model Fitted Values}
\usage{
fitted.values(x, \dots)
fitted(x, \dots)
}
\alias{fitted.values}
\alias{fitted}
\arguments{
\item{x}{an object for which the extraction of model
fitted values is meaningful.}
\item{\dots}{other arguments.}
}
\value{
Fitted values extracted from the object \code{x}.

This is a generic function which extracts fitted values
from objects returned by modeling functions.
The abbreviated form \code{fitted} is intended to encourage users
to access object components by using an accessor function
rather than by directly referencing an object slot.

All object classes which are returned by model fitting
functions should provide a \code{fitted.values} method.
}
\seealso{
\code{\link{coefficients}}, \code{\link{glm}}, \code{\link{lm}},
\code{\link{residuals}}.
}
\keyword{models}
\keyword{regression}
