\name{residuals}
\title{Extract Model Residuals}
\usage{
residuals(x, \dots)
resid(x, \dots)
}
\alias{residuals}
\alias{resid}
\arguments{
\item{x}{an object for which the extraction of model
residuals is meaningful.}
\item{\dots}{other arguments.}
}
\description{
\code{residuals} is a generic function which extracts
model residuals from objects returned by modeling functions.
The abbreviated form \code{resid} is intended to encourage users
to access object components through an accessor function
rather than by directly referencing an object slot.

All object classes which are returned by model fitting
functions should provide a \code{residuals} method.
}
\seealso{
\code{\link{coefficients}}, \code{\link{fitted.values}},
\code{\link{glm}}, \code{\link{lm}}.
}
\keyword{models}
\keyword{regression}
