\name{Exponential}
\alias{dexp}
\alias{pexp}
\alias{qexp}
\alias{rexp}
\title{The Exponential Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the exponential distribution with rate \code{rate}
  (i.e., mean \code{1/rate}).
}
\usage{
dexp(x, rate = 1)
pexp(q, rate = 1)
qexp(p, rate = 1)
rexp(n, rate = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations to generate.}
  \item{rate}{vector of rates.}
}
\value{
  \code{dexp} gives the density,
  \code{pexp} gives the distribution function,
  \code{qexp} gives the quantile function, and
  \code{rexp} generates random deviates.
}
\details{ 
  If \code{rate} is not specified, it assumes the default value of
  \code{1}.
  
  The exponential distribution with rate \eqn{\lambda} has density
  \deqn{
    f(x) = \lambda {e}^{- \lambda x}}{
    f(x) = lambda e^(- lambda x)}
  for \eqn{x \ge 0}.
}
\seealso{
  \code{\link{exp}} for the exponential function,
  \code{\link{dgamma}} for the gamma distribution and
  \code{\link{dweibull}} for the Weibull distribution, both of which
  generalize the exponential.
}
\examples{
dexp(1) - exp(-1) #-> 0
r <- rexp(100)
all(abs(1 - dexp(1, r) / (r*exp(-r))) < 1e-14)
}
\keyword{distribution}
