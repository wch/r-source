\name{Lognormal}
\alias{dlnorm}
\alias{plnorm}
\alias{qlnorm}
\alias{rlnorm}
\title{The Log Normal Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the log normal distribution whose logarithm has mean
  equal to \code{meanlog} and standard  deviation equal to \code{sdlog}.
}
\usage{
dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)                    
plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
qlnorm(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
rlnorm(n, meanlog = 0, sdlog = 1)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations to generate.}
  \item{meanlog, sdlog}{mean and standard deviation of the distribution
    on the log scale.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dlnorm} gives the density,
  \code{plnorm} gives the distribution function,
  \code{qlnorm} gives the quantile function, and
  \code{rlnorm} generates random deviates.
}
\details{
  If \code{meanlog} or \code{sdlog} are not specified they assume the
  default values of \code{0} and \code{1} respectively.

  The log normal distribution has density
  \deqn{
    f(x) = \frac{1}{\sqrt{2\pi}\sigma x} e^{-(\log(x) - \mu)^2/2 \sigma^2}%
  }{f(x) = 1/(sqrt(2 pi) sigma x) e^-((log x - mu)^2 / (2 sigma^2))}
  where \eqn{\mu} and \eqn{\sigma} are the mean and standard
  deviation of the logarithm.
}
\seealso{
  \code{\link{dnorm}} for the normal distribution.
}
\examples{
dlnorm(1) == dnorm(0)
x <- rlnorm(1000)       # not yet always :
all(abs(x  -  qlnorm(plnorm(x))) < 1e4 * .Machine$double.eps * x)
}
\keyword{distribution}
