\name{Hypergeometric}
\title{The Hypergeometric Distribution}
\usage{
dhyper(x, m, n, k)
phyper(q, m, n, k)
qhyper(p, m, n, k)
rhyper(nn, m, n, k)
}
\alias{dhyper}
\alias{phyper}
\alias{qhyper}
\alias{rhyper}
\arguments{
  \item{x,q}{vector of quantiles representing the number of white
  balls drawn without replacement from an urn which contains both
  black and white balls.}
  \item{m}{the number of white balls in the urn.}
  \item{n}{the number of black balls in the urn.}
  \item{k}{the number of balls drawn from the urn.}
  \item{p}{probability, it must be between 0 and 1.}
  \item{nn}{the number of observations to be generated.}
}
\value{
  These functions provide information about the hypergeometric
  distribution with parameters \code{m}, \code{n} and \code{k}
  (named \eqn{Np}, \eqn{N-Np}, and \eqn{n}, respectively in
   the reference below).
  \code{dhyper} gives the density, \code{phyper} gives the distribution
  function \code{qhyper} gives the quantile function and \code{rhyper}
  generates random deviates.

  The hypergeometric distribution is used for sampling \bold{without}
  replacement.  It has density
  \deqn{
    p(x) = \left. {m \choose x}{n \choose k-x} \right/ {m+n \choose k}%
  }{p(x) =	choose(m, x) choose(n, k-x) / choose(m+n, k)}
  for \eqn{x = 0, \ldots, k}{x = 0, ..., k}.
}
\details{
For large values of \code{k},
}
\references{
Johnson, N.L., Kotz, S. and Kemp, A.W. (1992).
\emph{Univariate Discrete Distributions}, 2nd Ed., Wiley.
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
