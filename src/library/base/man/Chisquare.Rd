\name{Chisquare}
\alias{Chisquare}
\alias{Chisquare}
\alias{dchisq}
\alias{pchisq}
\alias{qchisq}
\alias{rchisq}
\title{The (non-central) Chi-Squared Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the chi-squared (\eqn{\chi^2}{chi^2}) distribution with
  \code{df} degrees of freedom and optional non-centrality parameter
  \code{ncp}.
}
\usage{
dchisq(x, df, ncp=0, log = FALSE)
pchisq(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE)
qchisq(p, df, ncp=0, lower.tail = TRUE, log.p = FALSE)
rchisq(n, df, ncp=0)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{df}{degrees of freedom.}
  \item{ncp}{non-centrality parameter.  For \code{rnchisq}, \code{ncp=0}
    is the only possible value.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dchisq} gives the density, \code{pchisq} gives the distribution
  function, \code{qchisq} gives the quantile function, and \code{rchisq}
  generates random deviates.
}
\details{
  The chi-squared distribution with \code{df}\eqn{= n} degrees of
  freedom has density
  \deqn{f_n(x) = \frac{1}{{2}^{n/2} \Gamma (n/2)} {x}^{n/2-1} {e}^{-x/2}}{%
    f_n(x) = 1 / (2^(n/2) Gamma(n/2))  x^(n/2-1) e^(-x/2)}
  for \eqn{x > 0}.  The mean and variance are \eqn{n} and \eqn{2n}.

  The non-central chi-squared distribution with \code{df}\eqn{= n}
  degrees of freedom and non-centrality parameter \code{ncp}
  \eqn{= \lambda} has density
  \deqn{
    f(x) = e^{-\lambda / 2}
      \sum_{r=0}^\infty \frac{(\lambda/2)^r}{r!}\, f_{n + 2r}(x)}{%
    f(x) = exp(-lambda/2) SUM_{r=0}^infty ((lambda/2)^r / r!) dchisq(x, df + 2r)
  }
  for \eqn{x \ge 0}.  It is the distribution of the sum of squares of
  \eqn{n} normals each with variance one, \eqn{\lambda} being the sum of
  squares of the normal means.
}
\seealso{
  \code{\link{dgamma}} for the Gamma distribution which generalizes the
  chi-squared one.
}
\examples{
dchisq(1, df=1:3)
pchisq(1, df= 3)
pchisq(1, df= 3, ncp = 0:4)# includes the above

x <- 1:10
## Chi-squared(df = 2) is a special exponential distribution
all.equal(dchisq(x, df=2), dexp(x, 1/2))
all.equal(pchisq(x, df=2), pexp(x, 1/2))
}
\keyword{distribution}
