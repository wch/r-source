\name{NegBinomial}
\title{The Negative Binomial Distribution}
\usage{
dnbinom(x, size, prob)
pnbinom(q, size, prob)
qnbinom(p, size, prob)
rnbinom(n, size, prob)
}
\alias{dnbinom}
\alias{pnbinom}
\alias{qnbinom}
\alias{rnbinom}
\arguments{
\item{x,q}{vector of quantiles representing the number of failures
which occur in a sequence of Bernoulli trials before a target number of
successes is reached.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{size}{target for number of successful trials.}
\item{prob}{probability of success in each trial.}
}
\description{
  These functions provide information about the negative binomial
  distribution with parameters \code{size} and \code{prob}.  \code{dnbinom}
  gives the density, \code{pnbinom} gives the distribution function,
  \code{qnbinom} gives the quantile function and \code{rnbinom}
  generates random deviates.

  The negative binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{
    p(x) = {x+n-1 \choose x} p^n (1-p)^x}{
    p(x) = Choose(x+n-1,x) p^n (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}
}
\seealso{
  \code{\link{dbinom}} for the binomial, \code{\link{dpois}} for the
  Poisson and \code{\link{dgeom}} for the geometric distribution, which
  is a special case of the negative binomial.
}
\examples{
x <- 0:11
dnbinom(x, size = 1, prob = 1/2) * 2^(1 + x) # == 1
126 /  dnbinom(0:8, size  = 2, prob  = 1/2) #- theoretically integer

## Cumulative ('p') = Sum of discrete prob.s ('d');  Relative error :
summary(1 - cumsum(dnbinom(x, size = 2, prob = 1/2)) /
	          pnbinom(x, size  = 2, prob = 1/2))
}
\keyword{distribution}
