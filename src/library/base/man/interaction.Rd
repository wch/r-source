\name{interaction}
\title{Compute Factor Interactions}
\usage{
interaction(\dots, drop=FALSE)
}
\alias{interaction}
\arguments{
  \item{\dots}{The factors for which interaction is to be computed.}
  \item{drop}{If \code{drop} is \code{TRUE}, empty factor levels
    are dropped from the result. The default is to retain all
    factor levels.}
}
\description{
  \code{interaction} computes a factor which represents the interaction
  of the given factors.  The result of \code{interaction} is always unordered.
}
\value{
  A factor which represents the interaction of the given factors.
}
\seealso{\code{\link{factor}}.}%% even more ":" for factors!

\examples{
a <- gl(2, 2, 8)
b <- gl(2, 4, 8)
interaction(a, b)
}
\keyword{category}
