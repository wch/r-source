\name{Beta}
\title{The Beta Distribution}
\usage{
dbeta(x, shape1, shape2, ncp=0)
pbeta(q, shape1, shape2, ncp=0)
qbeta(p, shape1, shape2)
rbeta(n, shape1, shape2)
}
\alias{dbeta}
\alias{pbeta}
\alias{qbeta}
\alias{rbeta}
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{shape1,shape2}{positive parameters of the beta distribution.}
\item{ncp}{non-centrality parameter.}
}
\value{
These functions provide information about the Beta distribution
with parameters \code{shape1} and \code{shape2} (and optional non-centrality
parameter \code{ncp}).  \code{dbeta} gives the density,
\code{pbeta} the distribution function, \code{qbeta} the
quantile function and \code{rbeta} generates random deviates.

The Beta distribution with parameters \code{shape1} \eqn{= a} and \code{shape2}
\eqn{= b} has density
\deqn{f(x)=\frac{\Gamma (a + b )}{\Gamma (a) \Gamma (b)}{x}^{a} {(1-x)}^{b}%
}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}
for \eqn{a > 0}, \eqn{b > 0} and \eqn{0 < x < 1}.
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
