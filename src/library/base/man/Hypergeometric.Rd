\name{Hypergeometric}
\title{The Hypergeometric Distribution}
\usage{
dhyper(x, N1, N2, n)
phyper(q, N1, N2, n)
qhyper(p, N1, N2, n)
rhyper(nobs, N1, N2, n)
}
\alias{dhyper}
\alias{phyper}
\alias{qhyper}
\alias{rhyper}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{N1}{the number of white balls in the population.}
  \item{N2}{the number of black balls in the population.}
  \item{n}{the number of balls drawn from the urn.}
  \item{p}{probability, it must be between 0 and 1.}
  \item{nobs}{the number of observations to be generated.}
}
\value{
  These functions provide information about the hypergeometric
  distribution with parameters \code{N1}, \code{N2} and \code{n}.
  
  \code{dhyper} gives the density, \code{phyper} gives the distribution
  function \code{qhyper} gives the quantile function and \code{rhyper}
  generates random deviates.

  The hypergeometric distribution is used for sampling \bold{without}
  replacement.  It has density
  \deqn{
    p(x) =
    \left. {N1 \choose x}{N2 \choose n-x} \right/ {N1+N2 \choose n}}{
    p(x) =
    Choose(N1, x) Choose(N2, n-x) / Choose(N1+N2, n)}
  for \eqn{x = 0, \ldots, n}{x = 0, ..., n}.
}
\examples{
N1 <- 10; N2 <- 7; n <- 8
x <- 0:N1
rbind(phyper(x, N1, N2, n), dhyper(x, N1, N2, n))
all(phyper(x, N1, N2, n) == cumsum(dhyper(x, N1, N2, n)))
# FALSE
# Relative Error :
formatC(signif((phyper(x, N1, N2, n) / cumsum(dhyper(x, N1, N2, n)) - 1), 2),
        format='g', dig=2)
}
\keyword{distribution}
