\name{na.action}
\title{NA Action}
\usage{
na.action(x, \dots)
na.action.default(x)
}
\alias{na.action}
\alias{na.action.default}
\arguments{
  \item{x}{any object whose \code{\link{NA}} action is given.}
  \item{\dots}{further arguments special methods could assume.}
}
\description{
  \code{na.action} is a generic function, and \code{na.action.default} its
  default method.
}
\value{
  The ``NA action'' which should be applied to \code{x} whenever
  \code{\link{NA}}s are not desired.
}
\seealso{
  \code{\link{options}("na.action")}, \code{\link{na.omit}},
  \code{\link{na.fail}}
}
\examples{
na.action(c(1, NA))
}
\keyword{NA}
\keyword{methods}
