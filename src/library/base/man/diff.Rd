\name{diff}
\alias{diff}
\alias{diff.default}
\alias{diff.POSIXt}
\alias{diff.Date}
\title{Lagged Differences}
\description{
  Returns suitably lagged and iterated differences.
}
\usage{
diff(x, \dots)

\method{diff}{default}(x, lag = 1, differences = 1, \dots)

\method{diff}{POSIXt}(x, lag = 1, differences = 1, \dots)

\method{diff}{Date}(x, lag = 1, differences = 1, \dots)
}
\arguments{
  \item{x}{a numeric vector or matrix containing the values to be
    differenced.}
  \item{lag}{an integer indicating which lag to use.}
  \item{differences}{an integer indicating the order of the difference.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{diff} is a generic function with a default method and ones for
  classes \code{"\link{ts}"}, \code{"\link{POSIXt}"} and
  \code{"\link{Date}"}.

  \code{\link{NA}}'s propagate.
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{diff.ts}}, \code{\link{diffinv}}.
}
\examples{
diff(1:10, 2)
diff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
diff(x, lag = 2)
diff(x, differences = 2)

diff(.leap.seconds)
}
\keyword{arith}
