\name{Lognormal}
\alias{Lognormal}
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
\source{
  \code{dlnorm} is calculated from the definition (in Details).
  \code{[pqr]lnorm} are based on the relationship to the normal.
}
\details{
  The log normal distribution has density
  \deqn{
    f(x) = \frac{1}{\sqrt{2\pi}\sigma x} e^{-(\log(x) - \mu)^2/2 \sigma^2}%
  }{f(x) = 1/(sqrt(2 pi) sigma x) e^-((log x - mu)^2 / (2 sigma^2))}
  where \eqn{\mu} and \eqn{\sigma} are the mean and standard
  deviation of the logarithm.
  The mean is \eqn{E(X) = exp(\mu + 1/2 \sigma^2)}, and the variance
  \eqn{Var(X) = exp(2\mu + \sigma^2)(exp(\sigma^2) - 1)}{%
    Var(X) = exp(2*mu + sigma^2)*(exp(sigma^2) - 1)} and
  hence the coefficient of variation is
  \eqn{\sqrt{exp(\sigma^2) - 1}}{sqrt(exp(sigma^2) - 1)} which is
    approximately \eqn{\sigma} when that is small (e.g., \eqn{\sigma < 1/2}).
%% Mode = exp(max(0, mu - sigma^2)); Median = exp(mu)
}
\note{
  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is \code{-plnorm(t, r, lower = FALSE, log = TRUE)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 1, chapter 14.
  Wiley, New York.
}
\seealso{
  \code{\link{dnorm}} for the normal distribution.
}
\examples{
dlnorm(1) == dnorm(0)
}
\keyword{distribution}
