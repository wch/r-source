\name{Beta}
\title{The Beta Distribution}
\usage{
dbeta(x, a, b)
pbeta(q, a, b)
qbeta(p, a, b)
rbeta(n, a, b)
}
\alias{dbeta}
\alias{pbeta}
\alias{qbeta}
\alias{rbeta}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{a, b}{positive parameters of the beta distribution.}
}
\value{
These functions provide information about the Beta distribution
with parameters \code{a} and \code{b}.  \code{dbeta} gives the density,
\code{pbeta} the distribution function, \code{qbeta} the
quantile function and \code{rbeta} generates random deviates.

The Beta distribution has density
\deqn{f(x) =
\frac{\Gamma (a + b )}{\Gamma (a) \Gamma (b)}
{x}^{a} {(1-x)}^{b}}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}
for \eqn{a > 0},
\eqn{b > 0}
and
\eqn{0 < x < 1}.
}
\seealso{
\code{\link{beta}} for the beta function, and \code{\link{dgamma}} for the
Gamma distribution.
}
\examples{
x <- seq(0,1, length=21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)
}
\keyword{distribution}
