\name{Geometric}
\alias{dgeom}
\alias{pgeom}
\alias{qgeom}
\alias{rgeom}
\title{The Geometric Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the geometric distribution with parameter \code{prob}.
}
\usage{
dgeom(x, prob, log = FALSE)
pgeom(q, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
rgeom(n, prob)
}
\arguments{
  \item{x, q}{vector of quantiles representing the number of failures in
    a sequence of Bernoulli trials before success occurs.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{prob}{probability of success in each trial.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dgeom} gives the density,
  \code{pgeom} gives the distribution function,
  \code{qgeom} gives the quantile function, and
  \code{rgeom} generates random deviates.
}
\details{
  The geometric distribution with \code{prob} \eqn{= p} has density
  \deqn{p(x) = p {(1-p)}^{x}}{p(x) = p (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}{x = 0, 1, 2, ...}

  If an element of \code{x} is not integer, the result of \code{pgeom}
  is zero, with a warning.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
}
\seealso{
  \code{\link{dnbinom}} for the negative binomial which generalizes
  the geometric distribution.
}
\examples{
pp <- sort(c((1:9)/10, 1 - .2^(2:8)))
print(qg <- qgeom(pp, prob = .2))
## test that qgeom is an inverse of pgeom
print(qg1 <- qgeom(pgeom(qg, prob=.2), prob =.2))
all(qg == qg1)
Ni <- rgeom(20, prob = 1/4); table(factor(Ni, 0:max(Ni)))
}
\keyword{distribution}
