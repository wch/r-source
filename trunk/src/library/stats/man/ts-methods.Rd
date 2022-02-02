% File src/library/stats/man/ts-methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ts-methods}
\alias{diff.ts}
\alias{na.omit.ts}
\title{Methods for Time Series Objects}
\description{
  Methods for objects of class \code{"ts"}, typically the result of
  \code{\link{ts}}.
}
\usage{
\method{diff}{ts}(x, lag = 1, differences = 1, \dots)

\method{na.omit}{ts}(object, \dots)
}
\arguments{
  \item{x}{an object of class \code{"ts"} containing the values to be
    differenced.}
  \item{lag}{an integer indicating which lag to use.}
  \item{differences}{an integer indicating the order of the difference.}
  \item{object}{a univariate or multivariate time series.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  The \code{na.omit} method omits initial and final segments with
  missing values in one or more of the series.  \sQuote{Internal}
  missing values will lead to failure.
}
\value{
  For the \code{na.omit} method, a time series without missing values.
  The class of \code{object} will be preserved.
}
\seealso{
  \code{\link{diff}};
  \code{\link{na.omit}}, \code{\link{na.fail}},
  \code{\link{na.contiguous}}.
}
\keyword{ts}
