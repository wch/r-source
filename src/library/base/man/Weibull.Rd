\name{Weibull}
\title{The Weibull Distribution}
\usage{
dweibull(x, shape, scale = 1)
pweibull(q, shape, scale = 1)
qweibull(p, shape, scale = 1)
rweibull(n, shape, scale = 1)
}
\alias{dweibull}
\alias{pweibull}
\alias{qweibull}
\alias{rweibull}
\description{
  These functions provide information about the Weibull distribution
  with parameters \code{shape} and \code{scale}.  \code{dweibull} gives
  the density, \code{pweibull} gives the distribution function,
  \code{qweibull} gives the quantile function and \code{rweibull}
  generates random deviates.

  The Weibull distribution with \code{shape} parameter \eqn{a} and
  \code{scale} parameter \eqn{b} has density given by
  \deqn{
    f(x) =
    (a/b) {(x/b)}^{a-1} \exp (-{(x/b)}^{a})}{
    f(x) = (a/b) (x/b)^(a-1) exp(- (x/b)^a)}
  for \eqn{x > 0}.
}
\seealso{
  \code{\link{dexp}} for the Exponential which is a special case of a
  Weibull distribution. 
}
\examples{
x <- 1:10
dweibull(x, shape = 1) == dexp(x)
pweibull(x, shape = 1, scale = pi) == pexp(x, rate = 1/pi)
qweibull(x/11, shape = 1, scale = pi) == qexp(x/11, rate = 1/pi)
}
\keyword{distribution}
