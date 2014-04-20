% File src/library/stats/man/lag.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{lag}
\alias{lag}
\alias{lag.default}
\title{Lag a Time Series}
\description{
  Compute a lagged version of a time series, shifting the time base
  back by a given number of observations.
  
  \code{lag} is a generic function; this page documents its default
  method.
  }
\usage{
lag(x, \dots)

\method{lag}{default}(x, k = 1, \dots)
}
\arguments{
  \item{x}{A vector or matrix or univariate or multivariate time series}
  \item{k}{The number of lags (in units of observations).}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{  
  Vector or matrix arguments \code{x} are given a \code{tsp} attribute
  \emph{via} \code{\link{hasTsp}}.
}
\value{
  A time series object with the same class as \code{x}.
}

\note{
  Note the sign of \code{k}: a series lagged by a positive \code{k}
  starts \emph{earlier}.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{\code{\link{diff}}, \code{\link{deltat}} }

\examples{
lag(ldeaths, 12) # starts one year earlier
}
\keyword{ts}
