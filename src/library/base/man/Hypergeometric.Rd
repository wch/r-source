\name{Hypergeometric}
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
  \item{nn}{number of observations. If \code{length(nn) > 1}, the length
    is taken to be the number required.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dhyper} gives the density,
  \code{phyper} gives the distribution function,
  \code{qhyper} gives the quantile function, and
  \code{rhyper} generates random deviates.
}
\details{
  The hypergeometric distribution is used for sampling \emph{without}
  replacement.  The density of this distribution with parameters
  \code{m}, \code{n} and \code{k} (named \eqn{Np}, \eqn{N-Np}, and
  \eqn{n}, respectively in the reference below) is given by
  \deqn{
    p(x) = \left. {m \choose x}{n \choose k-x} \right/ {m+n \choose k}%
  }{p(x) =      choose(m, x) choose(n, k-x) / choose(m+n, k)}
  for \eqn{x = 0, \ldots, k}{x = 0, ..., k}.
}
\references{
  Johnson, N. L., Kotz, S., and Kemp, A. W. (1992)
  \emph{Univariate Discrete Distributions},
  Second Edition. New York: Wiley.
}
\examples{
m <- 10; n <- 7; k <- 8
x <- 0:m
rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))# FALSE
## Error :
signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), dig=3)
}
\keyword{distribution}
