\name{GammaDist}
\alias{GammaDist}
\alias{dgamma}
\alias{pgamma}
\alias{qgamma}
\alias{rgamma}
\concept{incomplete gamma function}
\title{The Gamma Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Gamma distribution with parameters \code{shape} and
  \code{scale}.
}
\usage{
dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
rgamma(n, shape, rate = 1, scale = 1/rate)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{rate}{an alternative way to specify the scale.}
  \item{shape, scale}{shape and scale parameters.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dgamma} gives the density,
  \code{pgamma} gives the distribution function
  \code{qgamma} gives the quantile function, and
  \code{rgamma} generates random deviates.
}
\details{
  If \code{scale} is omitted, it assumes the default value of \code{1}.

  The Gamma distribution with parameters \code{shape} \eqn{=\alpha}{= a}
  and \code{scale} \eqn{=\sigma}{= s} has density
  \deqn{
    f(x)= \frac{1}{{\sigma}^{\alpha}\Gamma(\alpha)} {x}^{\alpha-1} e^{-x/\sigma}%
  }{f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)}
  for \eqn{x > 0}, \eqn{\alpha > 0}{a > 0} and \eqn{\sigma > 0}{s > 0}.
  The mean and variance are
  \eqn{E(X) = \alpha\sigma}{E(X) = a*s} and
  \eqn{Var(X) = \alpha\sigma^2}{Var(X) = a*s^2}.

  \code{pgamma()} uses a new algorithm (mainly by Morten Welinder) which
  should be uniformly better or equal to AS 239, see the references.
}
\note{
  The S parametrization is via \code{shape} and \code{rate}: S has no
  \code{scale} parameter.

  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is \code{-pgamma(t, ..., lower = FALSE, log = TRUE)}.

  \code{pgamma} is closely related to the incomplete gamma function.  As
  defined by Abramowitz and Stegun 6.5.1
  \deqn{P(a,x) = \frac{1}{\Gamma(a)} \int_0^x t^{a-1} e^{-t} dt}{P(a,x) =
      1/Gamma(a) integral_0^x t^(a-1) exp(-t) dt}
  \eqn{P(a, x)} is \code{pgamma(x, a)}.  Other authors (for example
  Karl Pearson in his 1922 tables) omit the normalizing factor,
  defining the incomplete gamma function as \code{pgamma(x, a) * gamma(a)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Shea, B. L. (1988)
  Algorithm AS 239,  Chi-squared and Incomplete Gamma Integral,
  \emph{Applied Statistics (JRSS C)} \bold{37}, 466--473.

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.
}
\seealso{
  \code{\link{gamma}} for the Gamma function, \code{\link{dbeta}} for
  the Beta distribution and \code{\link{dchisq}} for the chi-squared
  distribution which is a special case of the Gamma distribution.
}
\examples{
-log(dgamma(1:4, shape=1))
p <- (1:9)/10
pgamma(qgamma(p,shape=2), shape=2)
1 - 1/exp(qgamma(p, shape=1))
}
\keyword{distribution}
