% File src/library/stats/man/Hypergeometric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Hypergeometric}
\alias{Hypergeometric}
\alias{dhyper}
\alias{phyper}
\alias{qhyper}
\alias{rhyper}
\title{The Hypergeometric Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the hypergeometric distribution.
}
\usage{
dhyper(x, m, n, k, log = FALSE)
phyper(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
qhyper(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
rhyper(nn, m, n, k)
}
\arguments{
  \item{x, q}{vector of quantiles representing the number of white balls
    drawn without replacement from an urn which contains both black and
    white balls.}
  \item{m}{the number of white balls in the urn.}
  \item{n}{the number of black balls in the urn.}
  \item{k}{the number of balls drawn from the urn.}
  \item{p}{probability, it must be between 0 and 1.}
  \item{nn}{number of observations.  If \code{length(nn) > 1}, the length
    is taken to be the number required.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dhyper} gives the density,
  \code{phyper} gives the distribution function,
  \code{qhyper} gives the quantile function, and
  \code{rhyper} generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rhyper}, and is the maximum of the lengths of the
  numerical arguments for the other functions.

  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\details{
  The hypergeometric distribution is used for sampling \emph{without}
  replacement.  The density of this distribution with parameters
  \code{m}, \code{n} and \code{k} (named \eqn{Np}, \eqn{N-Np}, and
  \eqn{n}, respectively in the reference below) is given by
  \deqn{
    p(x) = \left. {m \choose x}{n \choose k-x} \right/ {m+n \choose k}%
  }{p(x) =      choose(m, x) choose(n, k-x) / choose(m+n, k)}
  for \eqn{x = 0, \ldots, k}.

  Note that \eqn{p(x)} is non-zero only for
  \eqn{\max(0, k-n) \le x \le \min(k, m)}{max(0, k-n) <= x <= min(k, m)}.

  With \eqn{p := m/(m+n)} (hence \eqn{Np = N \times p} in the
  reference's notation), the first two moments are mean
  \deqn{E[X] = \mu = k p} and variance
  \deqn{\mbox{Var}(X) = k p (1 - p) \frac{m+n-k}{m+n-1},}{%
               Var(X) = k p (1 - p) * (m+n-k)/(m+n-1),}
  which shows the closeness to the Binomial\eqn{(k,p)} (where the
  hypergeometric has smaller variance unless \eqn{k = 1}).

  The quantile is defined as the smallest value \eqn{x} such that
  \eqn{F(x) \ge p}, where \eqn{F} is the distribution function.

  If one of \eqn{m, n, k}, exceeds \code{\link{.Machine}$integer.max},
  currently the equivalent of \code{qhyper(runif(nn), m,n,k)} is used,
  when a binomial approximation may be considerably more efficient.
}
\source{
  \code{dhyper} computes via binomial probabilities, using code
  contributed by Catherine Loader (see \code{\link{dbinom}}).

  \code{phyper} is based on calculating \code{dhyper} and
  \code{phyper(...)/dhyper(...)} (as a summation), based on ideas of Ian
  Smith and Morten Welinder.

  \code{qhyper} is based on inversion.

  \code{rhyper} is based on a corrected version of

  Kachitvichyanukul, V. and Schmeiser, B. (1985).
  Computer generation of hypergeometric random variates.
  \emph{Journal of Statistical Computation and Simulation},
  \bold{22}, 127--145.
}
\references{
  Johnson, N. L., Kotz, S., and Kemp, A. W. (1992)
  \emph{Univariate Discrete Distributions},
  Second Edition. New York: Wiley.
}
\seealso{
  \link{Distributions} for other standard distributions.
}
\examples{
m <- 10; n <- 7; k <- 8
x <- 0:(k+1)
rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
\donttest{## but error is very small:
signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = 3)
}}
\keyword{distribution}
