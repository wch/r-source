\name{diff}
\alias{diff}
\alias{diff.default}
\title{Lagged Differences}
\description{
  Returns suitably lagged and iterated differences.
}
\usage{
diff(x, \dots)
diff.default(x, lag=1, differences=1, \dots)
}
\arguments{
  \item{x}{a numeric vector or matrix containing the values to be
    differenced.}
  \item{lag}{an integer indicating which lag to use.}
  \item{differences}{an integer indicating the order of the difference.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{diff} is a generic function with a default method and one for
  class \code{\link{ts}} objects.  \code{\link{NA}}'s propagate.
}
\value{
  If \code{x} is a vector of length \code{n} and \code{differences=1},
  then the computed result is equal to the successive differences
  \code{x[(1+lag):n] - x[1:(n-lag)]}.

  If \code{difference} is larger than one this algorithm is applied
  recursively to \code{x}.
  Note that the returned value is a vector which is shorter than
  \code{x}.

  If \code{x} is a matrix then the difference operations are carried out
  on each column separately.
}
\seealso{
  \code{\link{diff.ts}}.
}
\examples{
diff(1:10, 2)
diff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
diff(x, lag = 2)
diff(x, differences = 2)
}
\keyword{ts}
\keyword{arith}
