\name{Normal}
\title{The Normal Distribution}
\usage{
dnorm(x, mean=0, sd=1)
pnorm(q, mean=0, sd=1)
qnorm(p, mean=0, sd=1)
rnorm(n, mean=0, sd=1)
}
\alias{dnorm}
\alias{pnorm}
\alias{qnorm}
\alias{rnorm}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilites.}
  \item{n}{number of observations.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
}
\description{
  These functions provide information about the normal distribution with
  mean equal to \code{mean} and standard deviation equal to \code{sd}.
  \code{dnorm} gives the density, \code{pnorm} gives the distribution
  function \code{qnorm} gives the quantile function and \code{rnorm}
  generates random deviates. 

  If \code{mean} or \code{sd} are not specified they assume the default
  values of \code{0} and \code{1}, respectively. 

  The normal distribution has density
  \deqn{
    f(x) =
    \frac{1}{\sqrt{2\pi}\sigma} e^{-(x-\mu)^2/2\sigma^2}}{
    f(x) = 1/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))
  }
  where \eqn{\mu}{mu} is the mean of the distribution and
  \eqn{\sigma}{sigma} the standard deviation.
}
\seealso{
  \code{\link{dlnorm}} for the \emph{Log}normal distribution.
}
\examples{
dnorm(0) == 1/ sqrt(2*pi)
dnorm(1) == exp(-1/2)/ sqrt(2*pi)
dnorm(1) == 1/ sqrt(2*pi*exp(1))
}
\keyword{distribution}
