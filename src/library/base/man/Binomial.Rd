\name{Binomial}
\title{The Binomial Distribution}
\usage{
dbinom(x, size, prob)
pbinom(q, size, prob)
qbinom(p, size, prob)
rbinom(n, size, prob)
}
\alias{dbinom}
\alias{pbinom}
\alias{qbinom}
\alias{rbinom}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{size}{number of trials.}
\item{prob}{probability of success on each trial.}
}
\description{
These functions provide information about the binomial distribution
with parameters \code{size} and \code{prob}.  \code{dbinom} gives the density,
\code{pbinom} gives the distribution function \code{qbinom} gives
the quantile function and \code{rbinom} generates random deviates.
}
\details{
The binomial distribution with \code{size} \eqn{= n} and
\code{prob} \eqn{= p} has density
\deqn{p(x) = {n \choose x} {p}^{x} {(1-p)}^{n-x}%
    }{p(x) = Choose(n,x) p^x (1-p)^(n-x)}
for \eqn{x = 0, \ldots, n}.

If an element of \code{x} is not integer, the result of \code{dbinom} is zero,
with a warning.

The quantile is left continuous: \code{qbinom(q, \dots)} is the largest
integer x such that P(X <= x) < q.
}

\seealso{
\code{\link{dnbinom}} for the negative binomial, and \code{\link{dpois}}
for the Poisson distribution.
}

\examples{
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))
}
\keyword{distribution}
