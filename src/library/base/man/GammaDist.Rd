\name{GammaDist}
\title{The Gamma Distribution}
\usage{
dgamma(x, shape, scale=1)
pgamma(q, shape, scale=1)
qgamma(p, shape, scale=1)
rgamma(n, shape, scale=1)
}
\alias{dgamma}
\alias{pgamma}
\alias{qgamma}
\alias{rgamma}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations.}
\item{shape, scale}{shape and scale parameters.}
}
\value{
These functions provide information about the gamma
distribution with parameters \code{shape} and \code{scale}.
\code{dgamma} gives the density,
\code{pgamma} gives the distribution function
\code{qgamma} gives the quantile function
and
\code{rgamma} generates random deviates.

If \code{scale} is omitted, it assumes the default value of \code{1}.

The gamma distribution with parameters \code{shape} \eqn{= \alpha}{= a} and
\code{scale} \eqn{= \sigma}{= b} has density
\deqn{
  f(x) = \frac{1}{{\sigma}^{\alpha}\Gamma(\alpha)} {x}^{\alpha-1} e^{-x/\sigma}%
}{f(x) = 1/(b^a Gamma(a)) x^(a-1) e^-(x/b)}
for \eqn{x > 0}, \eqn{\alpha > 0}{a > 0} and \eqn{\sigma > 0}{b > 0}.
}
\seealso{
\code{\link{gamma}} for the gamma function, \code{\link{dbeta}} for
the beta distribution and \code{\link{dchisq}} for the chi-square
distribution which is a special case of the gamma distribution.
}
\examples{
-log(dgamma(1:4, shape=1))
p <- (1:9)/10
pgamma(qgamma(p,shape=2), shape=2)
1 - 1/exp(qgamma(p, shape=1))
}
\keyword{distribution}
