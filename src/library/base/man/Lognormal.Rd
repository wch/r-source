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
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{meanlog, sdlog}{mean and standard deviation of the distribution
    on the log scale with default values of \code{0} and \code{1} respectively.}
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
  The log normal distribution has density
  \deqn{
    f(x) = \frac{1}{\sqrt{2\pi}\sigma x} e^{-(\log(x) - \mu)^2/2 \sigma^2}%
  }{f(x) = 1/(sqrt(2 pi) sigma x) e^-((log x - mu)^2 / (2 sigma^2))}
  where \eqn{\mu} and \eqn{\sigma} are the mean and standard
  deviation of the logarithm.
%% Mode = exp(max(0, mu - sigma^2)); Median = exp(mu)
%% E[X]   = exp(mu + 1/2 sigma^2);
%% Var[X] = exp(sigma^2) (exp(sigma^2) - 1) exp(2 mu)
%% -> coeff.var. = sigma/mu = sqrt(exp(sigma^2) - 1) ~= sigma, if sigma < 1/2
}
\note{
  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is \code{-plnorm(t, r, lower = FALSE, log = TRUE)}.
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
