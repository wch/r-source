\name{Weibull}
\alias{dweibull}
\alias{pweibull}
\alias{qweibull}
\alias{rweibull}
\title{The Weibull Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Weibull distribution with parameters \code{shape}
  and \code{scale}.
}
\usage{
dweibull(x, shape, scale = 1, log = FALSE)
pweibull(q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)
qweibull(p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE)
rweibull(n, shape, scale = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations to generate.}
  \item{shape, scale}{shape and scale parameters.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dweibull} gives the density,
  \code{pweibull} gives the distribution function,
  \code{qweibull} gives the quantile function, and
  \code{rweibull} generates random deviates.
}
\details{
  If \code{scale} is omitted it assumes the default value of \code{1}.

  The Weibull distribution with \code{shape} parameter \eqn{a} and
  \code{scale} parameter \eqn{b} has density given by
  \deqn{f(x) = (a/b) {(x/b)}^{a-1} \exp (-{(x/b)}^{a})}{%
        f(x) = (a/b) (x/b)^(a-1) exp(- (x/b)^a)}
  for \eqn{x > 0}.
}
\note{
  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is \code{-pweibull(t, r, lower = FALSE, log = TRUE)}.
}
\seealso{
  \code{\link{dexp}} for the Exponential which is a special case of a
  Weibull distribution.
}
\examples{
x <- 1:10
all.equal(dweibull(x, shape = 1), dexp(x))
all.equal(pweibull(x, shape = 1, scale = pi), pexp(x, rate = 1/pi))
all.equal(qweibull(x/11, shape = 1, scale = pi), qexp(x/11, rate = 1/pi))
}
\keyword{distribution}
