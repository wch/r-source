\name{coefficients}
\title{Extract Model Coefficients}
\usage{
coef(x, \dots)
coefficients(x, \dots)
}
\alias{coef}
\alias{coefficients}
\alias{coef.default}
\alias{coef.lm}
\alias{coef.glm}
\alias{coef.aov}
\alias{coef.listof}
\arguments{
  \item{x}{an object for which the extraction of model coefficients is
    meaningful.}
  \item{\dots}{other arguments.}
}
\value{
  Coefficients extracted from the model object \code{x}.
}
\description{
  \code{coef} is a generic function which extracts model coefficients
  from objects returned by modeling functions.  \code{coefficients} is
  an \emph{alias} (\code{\link{.Alias}}) for it.
}
\details{
  All object classes which are returned by model fitting functions
  should provide a \code{coef} method.  (Note that the method is \code{coef} and
  not \code{coefficients}.)
}
\seealso{
  \code{\link{fitted.values}} and \code{\link{residuals}} for related methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.
}
\examples{
x <- 1:5; coef(lm(c(1:3,7,6) ~ x))
}
\keyword{regression}
\keyword{models}
