\name{NegBinomial}
\alias{dnbinom}
\alias{pnbinom}
\alias{qnbinom}
\alias{rnbinom}
\title{The Negative Binomial Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the negative binomial distribution with parameters
  \code{size} and \code{prob}.
}
\usage{
dnbinom(x, size, prob, log = FALSE)
pnbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qnbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
rnbinom(n, size, prob)
}
\arguments{
  \item{x, q}{vector of quantiles representing the number of failures
    which occur in a sequence of Bernoulli trials before a target number
    of successes is reached, or alternately the probability distribution
    of a compound Poisson process whose intensity is distributed as a
    gamma (\code{\link{pgamma}}) distribution with scale parameter
    \code{(1-prob)/prob} and shape parameter \code{size} (this
    definition allows non-integer values of \code{size}).}
  \item{x}{vector of (non-negative integer) quantiles.}
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{size}{target for number of successful trials, or shape parameter
    of gamma distribution.}
  \item{prob}{probability of success in each trial, or scale of gamma
    distribution (\code{prob} = \code{scale/(1+scale)}).}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dnbinom} gives the density,
  \code{pnbinom} gives the distribution function,
  \code{qnbinom} gives the quantile function, and
  \code{rnbinom} generates random deviates.
}
\details{
  The negative binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{
    p(x) = \frac{\Gamma(x+n)}{\Gamma(n) x!} p^n (1-p)^x}{%
    p(x) = Gamma(x+n)/(Gamma(n) x!) p^n (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}

  If an element of \code{x} is not integer, the result of \code{dnbinom}
  is zero, with a warning.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
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

x <- 0:15
size <- (1:20)/4
persp(x,size, dnb <- outer(x,size,function(x,s)dnbinom(x,s, pr= 0.4)),
      xlab = "x", ylab = "s", zlab="density", theta = 150)
title(tit <- "negative binomial density(x,s, pr = 0.4)  vs.  x & s")

image  (x,size, log10(dnb), main= paste("log [",tit,"]"))
contour(x,size, log10(dnb),add=TRUE)
}
\keyword{distribution}
