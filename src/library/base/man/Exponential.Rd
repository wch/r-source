\name{Exponential}
\title{The Exponential Distribution}
\usage{
dexp(x, rate = 1)
pexp(q, rate = 1)
qexp(p, rate = 1)
rexp(n, rate = 1)
}
\alias{dexp}
\alias{pexp}
\alias{qexp}
\alias{rexp}
\arguments{
  \item{x q}{vector of quantiles}
  \item{p}{vector of probabilities}
  \item{n}{number of observations to generate}
  \item{rate}{vector of rates}
}
\value{
  These functions provide information about the exponential distribution
  with rate \code{rate} (i.e., mean \code{1/rate}). 
  \code{dexp} gives the density,
  \code{pexp} gives the distribution function,
  \code{qexp} gives the quantile function
  and
  \code{rexp} generates random deviates.

  The exponential distribution with rate \eqn{\lambda} has density
  \deqn{
    f(x) = \lambda {e}^{- \lambda x}}{
    f(x) = lambda e^(- lambda x)}
  for \eqn{x \ge 0}.
}
\seealso{
  \code{\link{exp}}, \code{\link{dchisq}} for the chisquare and
  \code{\link{dweibull}} for the Weibull distribution which both
  generalize the exponential.
}
\examples{
dexp(1) - exp(-1) #-> 0
r <- rexp(100)
all(abs(1 - dexp(1, r) / (r*exp(-r))) < 1e-14)
}
\keyword{distribution}
