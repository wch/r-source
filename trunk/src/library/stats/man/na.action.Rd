\name{na.action}
\title{NA Action}
\usage{
na.action(object, \dots)
%na.action.default(object, \dots)
}
\alias{na.action}
\alias{na.action.default}
\arguments{
  \item{object}{any object whose \code{\link{NA}} action is given.}
  \item{\dots}{further arguments special methods could require.}
}
\description{
  \code{na.action} is a generic function, and \code{na.action.default} its
  default method.
}
\value{
  The \dQuote{NA action} which should be applied to \code{object} whenever
  \code{\link{NA}}s are not desired.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{options}("na.action")}, \code{\link{na.omit}},
  \code{\link{na.fail}}, also for \code{na.exclude}, \code{na.pass}.
}
\examples{
na.action(c(1, NA))
}
\keyword{NA}
\keyword{methods}
