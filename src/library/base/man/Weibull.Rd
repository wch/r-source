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
\arguments{
\item{x,q}{vector of quantiles.}
\item{p}{vector of probabilities.}
\item{n}{number of observations to generate.}
\item{shape,scale}{shape and scale parameters.}
}
\description{
  These functions provide information about the Weibull distribution
  with parameters \code{shape} and \code{scale}.  \code{dweibull} gives
  the density, \code{pweibull} gives the distribution function,
  \code{qweibull} gives the quantile function and \code{rweibull}
  generates random deviates.

  If \code{scale} is omitted it assumes the default value of \code{1}.

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
all.equal(dweibull(x, shape = 1), dexp(x))
all.equal(pweibull(x, shape = 1, scale = pi), pexp(x, rate = 1/pi))
all.equal(qweibull(x/11, shape = 1, scale = pi), qexp(x/11, rate = 1/pi))
}
\keyword{distribution}
