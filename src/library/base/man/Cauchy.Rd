\name{Cauchy}
\title{The Cauchy Distribution}
\usage{
dcauchy(x, location = 0, scale = 1)
pcauchy(q, location = 0, scale = 1)
qcauchy(p, location = 0, scale = 1)
rcauchy(n, location = 0, scale = 1)
}
\alias{dcauchy}
\alias{pcauchy}
\alias{qcauchy}
\alias{rcauchy}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{location,scale}{location and scale parameters.}
}
\value{
  These functions provide information about the Cauchy distribution with
  location parameter \code{location} and scale parameter \code{scale}.

  \code{dcauchy}, \code{pcauchy}, and \code{qcauchy} are respectively
  the density, distribution function and quantile function of the Cauchy
  distribution.  \code{rcauchy} generates random deviates from the
  Cauchy.

  If \code{location} or \code{scale} are not specified, they assume
  the default values of \code{0} and \code{1} respectively.

  The Cauchy distribution with location \eqn{l} and scale \eqn{s} has
  density
  \deqn{f(x) = \frac{1}{\pi s}
    \left( 1 + \left(\frac{x - l}{s}\right)^2 \right)^{-1}%
  }{f(x) = 1 / (pi s (1 + ((x-l)/s)^2))}
  for all \eqn{x}.
}
\seealso{
  \code{\link{dt}} for the t distribution which generalizes
  \code{dcauchy(*, l = 0, s = 1)}.
}
\examples{
dcauchy(-1:4) == 1 / (pi*(1 + (-1:4)^2))
}
\keyword{distribution}
