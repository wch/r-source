\name{model.frame}
\title{Extracting the ``Environment'' of a Model Formula}
\usage{
model.frame(formula, data, na.action, \dots)
model.frame.default(formula, data=sys.frame(sys.parent()), 
                    na.action = na.fail, \dots)
}
\alias{model.frame}
\alias{model.frame.default}
\arguments{
  \item{formula}{a model formula}
  \item{data}{\code{data.frame} or \code{environment} where the objects
    in formula are.}
  \item{na.action}{how \code{NA}s are treated.}
  \item{\dots}{further arguments such as.......}
}
\description{
  \code{model.frame} is a generic function, and
  \code{model.frame.default} its default method.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula}.
}
\seealso{
  \code{\link{model.frame.lm}} for the method used in \code{\link{lm}}.
}
\examples{
data(cars)
data.class(model.frame(dist ~ speed, data = cars))
}
\keyword{models}
