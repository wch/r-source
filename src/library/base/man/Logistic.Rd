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

  The Logistic distribution with \code{location} \eqn{= a} and
  \code{scale} \eqn{= b} has density
  \deqn{
    f(x) = \frac{1}{b} \frac{e^{(x-a)/b}}{(1 + e^{(x-a)/b})^2}%
  }{f(x) = 1/b exp((x-a)/b) (1 + exp((x-a)/b))^-2.}
}
\examples{
x <- 0:4
paste(dlogis(x, loc = 0)) == paste(exp(x) * (1 + exp(x))^-2)
}
\keyword{distribution}
