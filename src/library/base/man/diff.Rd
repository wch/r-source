\name{diff}
\title{Lagged Differences}
\usage{
diff(x, \dots)
diff.default(x, lag=1, differences=1)
}
\alias{diff}
\alias{diff.default}
\arguments{
\item{x}{a numeric vector or matrix containing the values to be differenced.}
\item{lag}{an integer indicating which lag to use.}
\item{differences}{an integer indicating the order of the difference.}
}
\value{
If \code{x} is a vector of length \code{n} and \code{differences=1},
then the computed result is equal to the successive differences
\code{x[(1:n-lag)]-x[(lag:n)]}.
If \code{difference} is larger than one this
algorithm is applied recursively to \code{x}. Note that the returned value
is a vector which is shorter than \code{x}.

If \code{x} is a matrix then the difference operations are carried out
on each column separately.

\code{NA}'s propagate.
}
\examples{
diff(1:10, 2)
diff(1:10, 2, 2)
}
\keyword{ts}
\keyword{arith}

