\name{Binomial}
\title{The Binomial Distribution}
\usage{
dbinom(x, n, p)
pbinom(q, n, p)
qbinom(prob, n, p)
rbinom(nobs, n, p)
}
\alias{dbinom}
\alias{pbinom}
\alias{qbinom}
\alias{rbinom}
\arguments{
\item{x,q}{vector of quantiles.}
\item{prob}{vector of probabilities.}
\item{nobs}{number of observations to generate.}
\item{n}{number of trials.}
\item{p}{probability of success on each trial.}
}
\value{
These functions provide information about the binomial distribution
with parameters \code{n} and \code{p}.  \code{dbinom} gives the density,
\code{pbinom} gives the distribution function \code{qbinom} gives
the quantile function and \code{rbinom} generates random deviates.

The binomial distribution has density
\deqn{p(x) = {n \choose x} {p}^{x} {(1-p)}^{n-x}}{p(x) = Choose(n,x) p^x (1-p)^(n-x)}
for \eqn{x = 0, \ldots, n}.
}
\examples{
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))
}
\keyword{distribution}
