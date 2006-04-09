\name{Binomial}
\alias{Binomial}
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
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{size}{number of trials (zero or more).}
  \item{prob}{probability of success on each trial.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dbinom} gives the density, \code{pbinom} gives the distribution
  function, \code{qbinom} gives the quantile function and \code{rbinom}
  generates random deviates.

  If \code{size} is not an integer, \code{NaN} is returned.
}
\details{
  The binomial distribution with \code{size} \eqn{= n} and
  \code{prob} \eqn{= p} has density
  \deqn{p(x) = {n \choose x} {p}^{x} {(1-p)}^{n-x}}{
    p(x) = choose(n,x) p^x (1-p)^(n-x)}
  for \eqn{x = 0, \ldots, n}.

  If an element of \code{x} is not integer, the result of \code{dbinom}
  is zero, with a warning.
  \eqn{p(x)} is computed using Loader's algorithm, see the reference below.

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.
}
\seealso{
  \code{\link{dnbinom}} for the negative binomial, and
  \code{\link{dpois}} for the Poisson distribution.
}
\source{
  For \code{dbinom} a saddle-point expansion is used: see
  
  Catherine Loader (2000). \emph{Fast and Accurate Computation of
    Binomial Probabilities}; available from
  \url{http://www.herine.net/stat/software/dbinom.html}.

  \code{pbinom} uses \code{\link{pbeta}}.

  \code{qbinom} uses the Cornish--Fisher Expansion to include a skewness
  correction to a normal approximation, followed by a search.

  \code{rbinom} is based on

  Kachitvichyanukul, V. and Schmeiser, B. W. (1988)
  Binomial random variate generation.
  \emph{Communications of the ACM}, \bold{31}, 216--222.
}
\examples{
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log=TRUE), type='l', ylab="log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col='red', lwd=2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj=0)
mtext("extended range", adj=0, line = -1, font=4)
mtext("log(dbinom(k))", col="red", adj=1)
}
\keyword{distribution}
