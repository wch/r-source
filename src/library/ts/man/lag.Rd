\name{lag}
\alias{lag}
\alias{lag.default}
\title{Lag a Time Series}
\description{
  Computed a lagged version of a time series, shifting the time base
  back by a given number of observations.
}
\usage{
\method{lag}{default}(x, k = 1, \dots)
}
\arguments{
  \item{x}{A vector or matrix or univariate or multivariate time series}
  \item{k}{The number of lags (in units of observations).}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  Vector or matrix arguments \code{x} are coerced to time series.

  \code{lag} is a generic function; this page documents its default
  method.
}
\value{
  A time series object.
}
\author{B.D. Ripley}

\note{
  Note the sign of \code{k}: a series lagged by a positive \code{k}
  starts \emph{earlier}.
}

\seealso{\code{\link{diff}}, \code{\link{deltat}} }

\examples{
data(UKLungDeaths)
lag(ldeaths, 12) # starts one year earlier
}
\keyword{ts}
