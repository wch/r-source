\name{na.omit.ts}
\alias{na.omit.ts}
\alias{na.contiguous}
\title{NA Handling Routines for Time Series}
\usage{
na.contiguous(frame)
na.omit.ts(frame)
}
\arguments{
  \item{frame}{a univariate or multivariate time series.}
}
\description{
  For \code{na.omit.ts}, initial and final segments with missing
  values in one or more of the series are omitted. `Internal'
  missing values will lead to failure.

  For \code{na.contiguous} the longest consecutive stretch of
  non-missing values is used. (In the event of a tie, the first such
  stretch.)
}
\value{
  A time series without missing values. The class of \code{frame} will
  be preserved.
}
\author{B. D. Ripley}
\seealso{\code{\link{na.omit}}, \code{\link{na.fail}}}

\examples{
data(BJsales)
sales1 <- ts.union(BJsales, lead3 = lag(BJsales.lead, -3))
na.omit.ts(sales1)

data(presidents)
na.contiguous(presidents)
}
\keyword{ts}
