\name{Geometric}
\title{The Geometric Distribution}
\usage{
dgeom(x, prob)
pgeom(q, prob)
qgeom(p, prob)
rgeom(n, prob)
}
\alias{dgeom}
\alias{pgeom}
\alias{qgeom}
\alias{rgeom}
\arguments{
\item{x,q}{vector of quantiles representing the number of failures in
a sequence of Bernoulli trials before success occurs.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{prob}{probability of success in each trial.} 
}
\value{
  These functions provide information about the geometric distribution
  with parameter \code{prob}.  \code{dgeom} gives the density, \code{pgeom}
  gives the distribution function, \code{qgeom} gives the quantile
  function, and \code{rgeom} generates random deviates.

  The geometric distribution with \code{prob} \eqn{= p} has density
  \deqn{p(x) = p {(1-p)}^{x}}{p(x) = p (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}{x = 0, 1, 2, ...}
}
\seealso{
\code{\link{dnbinom}} for the negative binomial which generalizes
the geometric distribution.
}
\examples{
pp <- sort(c((1:9)/10, 1 - .2^(2:8)))
print(qg <- qgeom(pp, prob = .2))
for(i in 1:2) print(qg <- qgeom(pgeom(qg, prob=.2), prob =.2))
Ni <- rgeom(20, prob = 1/4); table(factor(Ni, 0:max(Ni)))
}
\keyword{distribution}
