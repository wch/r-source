\name{Binomial}
\alias{dbinom}
\alias{pbinom}
\alias{qbinom}
\alias{rbinom}
\title{The Binomial Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the binomial distribution with parameters \code{size}
  and \code{prob}.
}
\usage{
dbinom(x, size, prob, log = FALSE)
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
rbinom(n, size, prob)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations to generate.}
  \item{size}{number of trials.}
  \item{prob}{probability of success on each trial.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dbinom} gives the density, \code{pbinom} gives the distribution
  function, \code{qbinom} gives the quantile function and \code{rbinom}
  generates random deviates.
}
\details{
  The binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{p(x) = {n \choose x} {p}^{x} {(1-p)}^{n-x}}{
    p(x) = Choose(n,x) p^x (1-p)^(n-x)}
  for \eqn{x = 0, \ldots, n}.

  If an element of \code{x} is not integer, the result of \code{dbinom}
  is zero, with a warning.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
}

\seealso{
  \code{\link{dnbinom}} for the negative binomial, and
  \code{\link{dpois}} for the Poisson distribution.
}
\examples{
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
plot (0:n,     dbinom(0:n, n, pi/10, log=TRUE), type='l',
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(0:n, log(dbinom(0:n, n, pi/10)), col='red', lwd=2)
mtext("dbinom(k, log=TRUE)", adj=0)
mtext("extended range", adj=0, line = -1, font=4)
mtext("log(dbinom(k))", col="red", adj=1)
}
\keyword{distribution}
