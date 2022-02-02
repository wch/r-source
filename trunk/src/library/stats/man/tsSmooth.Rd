% File src/library/stats/man/tsSmooth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{tsSmooth}
\alias{tsSmooth}
\alias{tsSmooth.StructTS}
\title{Use Fixed-Interval Smoothing on Time Series}
\description{
  Performs fixed-interval smoothing on a univariate time series via a
  state-space model.  Fixed-interval smoothing gives the best estimate
  of the state at each time point based on the whole observed series.
}
\usage{
tsSmooth(object, \dots)
}
\arguments{
  \item{object}{a time-series fit.  Currently only class
    \code{"\link{StructTS}"} is supported}
  \item{\dots}{possible arguments for future methods.}
}
\value{
  A time series, with as many dimensions as the state space and results
  at each time point of the original series.  (For seasonal models, only
  the current seasonal component is returned.)
}

\references{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.
}

\author{ B. D. Ripley }

\seealso{
  \code{\link{KalmanSmooth}}, \code{\link{StructTS}}.

  For examples consult \code{\link{AirPassengers}},
  \code{\link{JohnsonJohnson}} and \code{\link{Nile}}.
}
\keyword{ts}

