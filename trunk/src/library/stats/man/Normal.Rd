\name{Normal}
\alias{Normal}
\alias{dnorm}
\alias{pnorm}
\alias{qnorm}
\alias{rnorm}
% These concepts are for the last example
\concept{error function}
\concept{erf}
\concept{erfc}
\title{The Normal Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the normal distribution with mean equal to \code{mean}
  and standard deviation equal to \code{sd}.
}
\usage{
dnorm(x, mean=0, sd=1, log = FALSE)
pnorm(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean=0, sd=1)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dnorm} gives the density,
  \code{pnorm} gives the distribution function,
  \code{qnorm} gives the quantile function, and
  \code{rnorm} generates random deviates.
}
\details{
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

  \code{qnorm} is based on Wichura's algorithm AS 241 which provides
  precise results up to about 16 digits.
}
\seealso{
  \code{\link{runif}} and \code{\link{.Random.seed}} about random number
  generation, and \code{\link{dlnorm}} for the \emph{Log}normal distribution.
}
\source{
  For \code{pnorm}, based on

  Cody, W. D. (1993)
  Algorithm 715: SPECFUN -- A portable FORTRAN package of special
  function routines and test drivers.
  \emph{ACM Transactions on Mathematical Software} \bold{19}, 22--32.

  For \code{qnorm}, the code is a C translation of

  Wichura, M. J. (1988)
  Algorithm AS 241: The Percentage Points of the Normal Distribution.
  \emph{Applied Statistics}, \bold{37}, 477--484.

  For \code{rnorm}, see \link{RNG} for how to select the algorithm and
  for references to the supplied methods.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 1, chapter 13.
  Wiley, New York.
}
\examples{
dnorm(0) == 1/ sqrt(2*pi)
dnorm(1) == exp(-1/2)/ sqrt(2*pi)
dnorm(1) == 1/ sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow=c(2,1))
plot(function(x) dnorm(x, log=TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add=TRUE, col="red",lwd=2)
mtext("dnorm(x, log=TRUE)", adj=0)
mtext("log(dnorm(x))", col="red", adj=1)

plot(function(x) pnorm(x, log=TRUE), -50, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add=TRUE, col="red",lwd=2)
mtext("pnorm(x, log=TRUE)", adj=0)
mtext("log(pnorm(x))", col="red", adj=1)

## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
}
\keyword{distribution}
