\name{fitted.values}
\title{Extract Model Fitted Values}
\usage{
fitted(x, \dots)
fitted.values(x, \dots)
}
\alias{fitted}
\alias{fitted.values}
\arguments{
  \item{x}{an object for which the extraction of model fitted values is
    meaningful.}
  \item{\dots}{other arguments.}
}
\value{
  Fitted values extracted from the object \code{x}.
}
\description{
  \code{fitted} is a generic function which extracts fitted values from
  objects returned by modeling functions.  \code{fitted.values} is an
  alias for it.

  All object classes which are returned by model fitting functions
  should provide a \code{fitted} method.  (Note that the method is
  `fitted' and not `fitted.values'.)
}
\seealso{
  \code{\link{coefficients}}, \code{\link{glm}}, \code{\link{lm}},
  \code{\link{residuals}}.
}
\keyword{models}
\keyword{regression}
