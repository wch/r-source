\name{Geometric}
\title{The Geometric Distribution}
\usage{
dgeom(x, p)
pgeom(q, p)
qgeom(prob, p)
rgeom(n, p)
}
\alias{dgeom}
\alias{pgeom}
\alias{qgeom}
\alias{rgeom}
\value{
  These functions provide information about the geometric distribution
  with parameter \code{p}.  \code{dgeom} gives the density, \code{pgeom}
  gives the distribution function, \code{qgeom} gives the quantile
  function, and \code{rgeom} generates random deviates.

  The geometric distribution has density
  \deqn{p(x) = p {(1-p)}^{x}}{p(x) = p (1-p)^x}
  for \eqn{x = 0, 1, 2, \ldots}{x = 0, 1, 2, ...}
}
\examples{
pp <- sort(c((1:9)/10, 1 - .2^(2:8)))
print(qg <- qgeom(pp, p = .2))
for(i in 1:2) print(qg <- qgeom(pgeom(qg, p=.2), p =.2))
Ni <- rgeom(20, p = 1/4); table(factor(Ni, 0:max(Ni)))
}
\keyword{distribution}
