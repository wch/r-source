\name{model.frame}
\title{Extracting the ``Environment'' of a Model Formula}
\usage{
model.frame(formula, data, na.action, \dots)
model.frame.default(formula, data=sys.frame(sys.parent()),
                    na.action = na.fail, \dots)

Methods for
	lm	glm	aovlist
}
\alias{model.frame}
\alias{model.frame.default}
\alias{model.frame.lm}
\alias{model.frame.glm}
\alias{model.frame.aovlist}
\arguments{
  \item{formula}{a model formula}
  \item{data}{\code{data.frame} or \code{environment} where the objects
    in formula are.}
  \item{na.action}{how \code{NA}s are treated.}
  \item{\dots}{further arguments such as.......}
}
\description{
  \code{model.frame} (generic function) and its methods return a
  \code{\link{data.frame}} with the variables from \code{formula}.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula}.
}
\seealso{\code{\link{model.matrix}} for the ``design matrix''.}
\examples{
data(cars)
data.class(model.frame(dist ~ speed, data = cars))
}
\keyword{models}
