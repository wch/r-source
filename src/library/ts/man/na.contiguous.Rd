\name{na.contiguous}
\alias{na.contiguous}
\title{NA Handling Routines for Time Series}
\description{
  Find the longest consecutive stretch of non-missing values in a time
  series object.  (In the event of a tie, the first such stretch.)
}
\usage{
na.contiguous(frame)
}
\arguments{
  \item{frame}{a univariate or multivariate time series.}
}
\value{
  A time series without missing values.  The class of \code{frame} will
  be preserved.
}
\author{B. D. Ripley}
\seealso{
  \code{\link{na.omit}} and \code{\link{na.omit.ts}};
  \code{\link{na.fail}}
}
\examples{
data(presidents)
na.contiguous(presidents)
}
\keyword{ts}
