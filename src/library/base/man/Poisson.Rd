\name{Poisson}
\title{The Poisson Distribution}
\usage{
dpois(x, lambda)
ppois(q, lambda)
qpois(p, lambda)
rpois(n, lambda)
}
\alias{dpois}
\alias{ppois}
\alias{qpois}
\alias{rpois}
\arguments{
\item{x}{vector of (positive) quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of random values to return.}
\item{lambda}{vector of positive means.}
}
\description{
These functions provide information about the Poisson distribution
with parameter \code{lambda}.  \code{dpois} gives the density,
\code{ppois} gives the distribution function \code{qpois} gives the
quantile function and \code{rpois} generates random deviates.

The Poisson distribution has density
\deqn{p(x) = \frac{{\lambda}^{x} {e}^{-\lambda}}{x!}}{p(x) = lambda^x exp(-lambda)/x!}
for \eqn{x = 0, 1, 2, \ldots}.
}
\seealso{
\code{\link{dbinom}} for the binomial and \code{\link{dnbinom}} for
the negative binomial distribution.
}
\examples{
-log(dpois(0:7, lambda=1) * gamma(1+ 0:7))
Ni <- rpois(50, lam= 4); table(factor(Ni, 0:max(Ni)))
}
\keyword{distribution}
