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
\item{x}{vector of (non-negative integer) quantiles.}
\item{q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of random values to return.}
\item{lambda}{vector of positive means.}
}
\description{
These functions provide information about the Poisson distribution
with parameter \code{lambda}.  \code{dpois} gives the density,
\code{ppois} gives the distribution function \code{qpois} gives the
quantile function and \code{rpois} generates random deviates.
}
\details{
The Poisson distribution has density
\deqn{p(x) = \frac{{\lambda}^{x} {e}^{-\lambda}}{x!}}{p(x) = lambda^x exp(-lambda)/x!}
for \eqn{x = 0, 1, 2, \ldots}.

If an element of \code{x} is not integer, the result of \code{dpois} is zero,
with a warning.
}
\seealso{
\code{\link{dbinom}} for the binomial and \code{\link{dnbinom}} for
the negative binomial distribution.
}
\examples{
-log(dpois(0:7, lambda=1) * gamma(1+ 0:7))
Ni <- rpois(50, lam= 4); table(factor(Ni, 0:max(Ni)))

par(mfrow = c(2, 1))
x <- seq(-0.01, 5, 0.01)
plot(x, ppois(x, 1), type="s", ylab="F(x)", main="Poisson(1) CDF")
plot(x, pbinom(x, 100, 0.01),type="s", ylab="F(x)",
     main="Binomial(100, 0.01) CDF")
\testonly{dpois(c(0, 1, 0.17, 0.77), 1)}
}
\keyword{distribution}
