\name{arima.sim}
\alias{arima.sim}
\title{Simulate from an ARIMA Model}
\description{
  Simulate from an ARIMA model.
}
\usage{
arima.sim(n, model, rand.gen=rnorm, innov=rand.gen(n, ...),
          n.start = 100, ...)
}
\arguments{
  \item{n}{length of output series.}
  \item{model}{A list with component \code{ar} and/or \code{ma} giving
    the AR and MA coeffcients respectively.}
  \item{rand.gen}{optional: a function to generate the innovations.}
  \item{innov}{an optional times series of innovations.  If not
    provided, \code{rand.gen} is used.}
  \item{n.start}{length of ``burn-in'' period.}
  \item{\dots}{additional arguments for \code{rand.gen}.}
}
\details{
  Only ARMA models are currently implemented. \code{\link{diffinv}} can
  easily be used to integrate them to from ARMA simulations.
}
\value{
  A time-series object of class \code{"ts"}.
}

\examples{
arima.sim(63, list(ar=c(0.8897, -0.4858), ma=c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) rnorm(n, sd=sqrt(0.1796)))
}
\keyword{ts}
