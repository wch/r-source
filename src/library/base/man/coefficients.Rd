\name{coefficients}
\title{Extract Model Coefficents}
\usage{
coef(x, \dots)
coefficients(x, \dots)
}
\alias{coef}
\alias{coefficients}
\arguments{
  \item{x}{an object for which the extraction of model coefficients is
    meaningful.}
  \item{\dots}{other arguments.}
}
\value{
  Coefficients extracted from the model object \code{x}.
}
\description{
  \code{coefficients} is a generic function which extract model
  coefficents from objects returned by modeling functions.

  The abbreviated form \code{coef} is intended to encourage users to
  access object components through an accessor function rather than by
  directly referencing an object slot (list element).

  All object classes which are returned by model fitting functions
  should provide a \code{coef} method.  (Note that method is `coef' and
  not `coefficients'.)
}
\seealso{
  \code{\link{fitted.values}} and \code{\link{residuals}} for related
  methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.
}
\keyword{regression}
\keyword{models}
