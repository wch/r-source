% File src/library/stats/man/na.contiguous.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{na.contiguous}
\alias{na.contiguous}
\alias{na.contiguous.default}
\title{Find Longest Contiguous Stretch of non-NAs}
\description{
  Find the longest consecutive stretch of non-missing values in a time
  series object.  (In the event of a tie, the first such stretch.)
}
\usage{
na.contiguous(object, \dots)
}
\arguments{
  \item{object}{a univariate or multivariate time series.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A time series without missing values.  The class of \code{object} will
  be preserved.
}
\seealso{
  \code{\link{na.omit}} and \code{\link{na.omit.ts}};
  \code{\link{na.fail}}
}
\examples{
na.contiguous(presidents)
}
\keyword{ts}
