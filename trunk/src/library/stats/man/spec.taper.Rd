% File src/library/stats/man/spec.taper.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{spec.taper}
\alias{spec.taper}
\title{Taper a Time Series by a Cosine Bell}
\usage{
spec.taper(x, p = 0.1)
}
\arguments{
 \item{x}{A univariate or multivariate time series}
 \item{p}{The proportion to be tapered at each end of the series,
   either a scalar (giving the proportion for all series)
   or a vector of the length of the number of series (giving the
   proportion for each series).}
}
\description{
  Apply a cosine-bell taper to a time series.
}
\details{
  The cosine-bell taper is applied to the first and last \code{p[i]}
  observations of time series \code{x[, i]}.
}
\value{
  A new time series object.
}
\seealso{
  \code{\link{spec.pgram}}, \code{\link{cpgram}}
}
\keyword{ts}
