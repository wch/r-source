\name{coefficients}
\title{Extract Model Coefficents}
\usage{
coefficients(x, \dots)
coef(x, \dots)
}
\alias{coefficients}
\alias{coef}
\arguments{
\item{x}{an object for which the extraction of model
coefficients is meaningful.}
\item{\dots}{other arguments.}
}
\value{
These are generic functions which extract
model coefficents from objects returned by modeling functions.
The abbreviated form \code{coef} is intended to encourage users
to access object components through an accessor function
rather than by directly referencing an object slot.

All object classes which are returned by model fitting
functions should provide a \code{coefficients} method.
}
\seealso{
\code{\link{fitted.values}}, \code{\link{glm}}, \code{\link{lm}},
\code{\link{residuals}}.
}
\keyword{regression}
\keyword{models}
