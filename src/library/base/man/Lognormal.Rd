\name{Lognormal}
\title{The Log Normal Distribution}
\usage{
dlnorm(x, meanlog = 0, sdlog = 1)
plnorm(q, meanlog = 0, sdlog = 1)
qlnorm(p, meanlog = 0, sdlog = 1)
rlnorm(n, meanlog = 0, sdlog = 1)
}
\alias{dlnorm}
\alias{plnorm}
\alias{qlnorm}
\alias{rlnorm}
\description{
  These functions provide information about the log normal distribution
  whose logarithm has mean equal to \code{meanlog} and standard
  deviation equal to \code{sdlog}.  \code{dlnorm} gives the density,
  \code{plnorm} gives the distribution function \code{qlnorm} gives the
  quantile function and \code{rlnorm} generates random deviates. 

  If \code{meanlog} or \code{sdlog} are not specified they assume the
  default values of \code{0} and \code{1} respectively.

  The log normal distribution has density
  \deqn{
    f(x) =
    \frac{1}{\sqrt{2\pi}\sigma x} e^{-(\log(x) - \mu)^2/2 \sigma^2}}{
    f(x) = 1/(sqrt(2 pi) sigma x) e^-((log x - mu)^2 / (2 sigma^2))
  }
  where \eqn{\mu}{mu} and \eqn{\sigma}{sigma} are the mean and standard
  deviation of the logarithm.
}
\seealso{
  \code{\link{dnorm}}, etc.
}
\keyword{distribution}
