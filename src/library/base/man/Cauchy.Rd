\name{Cauchy}
\alias{dcauchy}
\alias{pcauchy}
\alias{qcauchy}
\alias{rcauchy}
\title{The Cauchy Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Cauchy distribution with location parameter
  \code{location} and scale parameter \code{scale}.
}
\usage{
dcauchy(x, location = 0, scale = 1, log = FALSE)
pcauchy(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
qcauchy(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
rcauchy(n, location = 0, scale = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations to generate.}
  \item{location, scale}{location and scale parameters.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dcauchy}, \code{pcauchy}, and \code{qcauchy} are respectively
  the density, distribution function and quantile function of the Cauchy
  distribution.  \code{rcauchy} generates random deviates from the
  Cauchy.
}
\details{
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
all.equal(dcauchy(-1:4), 1 / (pi*(1 + (-1:4)^2)))
}
\keyword{distribution}
