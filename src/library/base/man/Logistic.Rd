\name{Logistic}
\title{The Logistic Distribution}
\usage{
dlogis(x, location=0, scale=1)
plogis(q, location=0, scale=1)
qlogis(p, location=0, scale=1)
rlogis(n, location=0, scale=1)
}
\alias{dlogis}
\alias{plogis}
\alias{qlogis}
\alias{rlogis}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{location,scale}{location and scale parameters.}
}

\description{
  These functions provide information about the logistic distribution
  with parameters \code{location} and \code{scale}.  \code{dlogis} gives
  the density, \code{plogis} gives the distribution function
  \code{qlogis} gives the quantile function and \code{rlogis} generates
  random deviates.

  If \code{location} or \code{scale} are omitted, they assume the
  default values of \code{0} and \code{1} respectively.

  The Logistic distribution with \code{location} \eqn{= \mu}{= m} and
  \code{scale} \eqn{= \sigma}{= s} has
  distribution function
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
all.equal(plogis(x, loc = 0), 1/(1 + exp(-x)),          tol = eps)
all.equal(dlogis(x, loc = 0), exp(x) * (1 + exp(x))^-2, tol = eps)

var(rlogis(4000, 0, s = 5))# approximately (+/- 3)
pi^2/3 * 5^2
}
\keyword{distribution}
