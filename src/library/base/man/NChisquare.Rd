\name{NChisquare}
\title{The Non-Central Chi-Square Distribution}
\usage{
dnchisq(x, df, lambda)
pnchisq(q, df, lambda)
qnchisq(p, df, lambda)
rnchisq(n, df, lambda)
}
\alias{dnchisq}
\alias{pnchisq}
\alias{qnchisq}
\alias{rnchisq}
\description{
  These functions provide information about the non-central chi-square
  distribution with \code{df} degrees of freedom and non-centrality
  parameter \code{lambda}.  \code{dnchisq} gives the density,
  \code{pnchisq} gives the distribution function, \code{qnchisq} gives
  the quantile function and \code{rnchisq} generates random deviates.

  The non-central chi-square distribution with \eqn{df} degrees of
  freedom and non-centrality parameter \eqn{\lambda} has density
  \deqn{
    f(x) = e^{-\lambda / 2} 
    \sum_{r=0}^\infty \frac{\lambda^r}{2^r r!}
    \, \mathrm{pchisq}(x, df + 2r)}{
    f(x) = exp(-lambda/2) SUM_{r=0}^infty (lambda^r / 2^r r!)
           pchisq(x, df + 2r)}
  for \eqn{x \ge 0}.
}
\seealso{
  \code{\link{dchisq}}, \code{\link{dgamma}}.
}
\examples{
pnchisq(1, df = 3, lambda = 0) == pchisq(1, df = 3)
pnchisq(1, df = 3, lambda = 0:4)
}
\keyword{distribution}
