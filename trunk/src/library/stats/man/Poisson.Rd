\name{Poisson}
\alias{Poisson}
\alias{dpois}
\alias{ppois}
\alias{qpois}
\alias{rpois}
\title{The Poisson Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Poisson distribution with parameter \code{lambda}.
}
\usage{
dpois(x, lambda, log = FALSE)
ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
rpois(n, lambda)
}
\arguments{
  \item{x}{vector of (non-negative integer) quantiles.}
  \item{q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of random values to return.}
  \item{lambda}{vector of (non-negative) means.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dpois} gives the (log) density,
  \code{ppois} gives the (log) distribution function,
  \code{qpois} gives the quantile function, and
  \code{rpois} generates random deviates.

  Invalid \code{lambda} will result in return value \code{NaN}, with a warning.
}
\details{
  The Poisson distribution has density
%-- please leave the linebreaking for the next two ! --
  \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}}{%
        p(x) = lambda^x exp(-lambda)/x!}
  for \eqn{x = 0, 1, 2, \ldots}.  The mean and variance are
  \eqn{E(X) = Var(X) = \lambda}.

  If an element of \code{x} is not integer, the result of \code{dpois}
  is zero, with a warning.
  \eqn{p(x)} is computed using Loader's algorithm, see the reference in
  \code{\link{dbinom}}.

  The quantile is left continuous: \code{qgeom(q, prob)} is the largest
  integer \eqn{x} such that \eqn{P(X \le x) < q}.

  Setting \code{lower.tail = FALSE} allows to get much more precise
  results when the default, \code{lower.tail = TRUE} would return 1, see
  the example below.
}
\source{
  \code{dpois} uses C code contributed by Catherine Loader
  (see \code{\link{dbinom}}).

  \code{ppois} uses \code{pgamma}.

  \code{qpois} uses the Cornish--Fisher Expansion to include a skewness
  correction to a normal approximation, followed by a search.

  \code{rpois} uses

   Ahrens, J. H. and Dieter, U. (1982).
   Computer generation of Poisson deviates from modified normal distributions.
   \emph{ACM Transactions on Mathematical Software}, \bold{8}, 163--179.
}
\seealso{
  \code{\link{dbinom}} for the binomial and \code{\link{dnbinom}} for
  the negative binomial distribution.
}
\examples{
-log(dpois(0:7, lambda=1) * gamma(1+ 0:7)) # == 1
Ni <- rpois(50, lam= 4); table(factor(Ni, 0:max(Ni)))

1 - ppois(10*(15:25), lambda=100)		# becomes 0 (cancellation)
    ppois(10*(15:25), lambda=100, lower=FALSE)	# no cancellation

par(mfrow = c(2, 1))
x <- seq(-0.01, 5, 0.01)
plot(x, ppois(x, 1), type="s", ylab="F(x)", main="Poisson(1) CDF")
plot(x, pbinom(x, 100, 0.01),type="s", ylab="F(x)",
     main="Binomial(100, 0.01) CDF")
}
\keyword{distribution}
