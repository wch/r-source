\name{Logistic}
\alias{dlogis}
\alias{plogis}
\alias{qlogis}
\alias{rlogis}
\title{The Logistic Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the logistic distribution with parameters
  \code{location} and \code{scale}.
}
\usage{
dlogis(x, location = 0, scale = 1, log = FALSE)
plogis(q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
qlogis(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE)
rlogis(n, location = 0, scale = 1)
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
  \code{dlogis} gives the density,
  \code{plogis} gives the distribution function,
  \code{qlogis} gives the quantile function, and
  \code{rlogis} generates random deviates.
}
\details{
  If \code{location} or \code{scale} are omitted, they assume the
  default values of \code{0} and \code{1} respectively.

  The Logistic distribution with \code{location} \eqn{= \mu}{= m} and
  \code{scale} \eqn{= \sigma}{= s} has distribution function
  \deqn{F(x) = \frac{1}{1 + e^{(x-\mu)/\sigma}}}{F(x) = 1 / (1 + exp(-(x-m)/s))}
  and density
  \deqn{
    f(x)= \frac{1}{\sigma}\frac{e^{(x-\mu)/\sigma}}{(1 + e^{(x-\mu)/\sigma})^2}%
  }{f(x) = 1/s exp((x-m)/s) (1 + exp((x-m)/s))^-2.}

  It is a long-tailed distribution with mean \eqn{\mu}{m} and variance
  \eqn{\pi^2/3 \sigma^2}{pi^2 /3 s^2}.
}
\examples{
eps <- 100 * .Machine$double.eps
x <- c(0:4, rlogis(100))
all.equal(plogis(x),                 1 / (1 + exp(-x)), tol = eps)
all.equal(plogis(x, lower=F),   exp(-x)/ (1 + exp(-x)), tol = eps)
all.equal(plogis(x, lower=F, log=T), -log(1 + exp(x)),  tol = eps)
all.equal(dlogis(x), exp(x) * (1 + exp(x))^-2, tol = eps)

var(rlogis(4000, 0, s = 5))# approximately (+/- 3)
pi^2/3 * 5^2
}
\keyword{distribution}
