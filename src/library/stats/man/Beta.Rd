\name{Beta}
\alias{Beta}
\alias{dbeta}
\alias{pbeta}
\alias{qbeta}
\alias{rbeta}
\title{The Beta Distribution}
\concept{incomplete beta function}
\description{
  Density, distribution function, quantile function and random
  generation for the Beta distribution with parameters \code{shape1} and
  \code{shape2} (and optional non-centrality parameter \code{ncp}).
}
\usage{
dbeta(x, shape1, shape2, ncp=0, log = FALSE)
pbeta(q, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE)
qbeta(p, shape1, shape2,        lower.tail = TRUE, log.p = FALSE)
rbeta(n, shape1, shape2)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{shape1, shape2}{positive parameters of the Beta distribution.}
  \item{ncp}{non-centrality parameter.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dbeta} gives the density, \code{pbeta} the distribution
  function, \code{qbeta} the quantile function, and \code{rbeta}
  generates random deviates.
}
\details{
  The Beta distribution with parameters \code{shape1} \eqn{= a} and
  \code{shape2} \eqn{= b} has density
  \deqn{f(x)=\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}{x}^{a} {(1-x)}^{b}%
  }{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}
  for \eqn{a > 0}, \eqn{b > 0} and \eqn{0 \le x \le 1}{0 <= x <= 1}
  where the boundary values at \eqn{x=0} or \eqn{x=1} are defined as
  by continuity (as limits).
  \cr
  The mean is \eqn{a/(a+b)} and the variance is \eqn{ab/((a+b)^2 (a+b+1))}.

  \code{pbeta} is closely related to the incomplete beta function.  As
  defined by Abramowitz and Stegun 6.6.1
  \deqn{B_x(a,b) = \int_0^x t^{a-1} (1-t)^{b-1} dt,}{B_x(a,b) =
    integral_0^x t^(a-1) (1-t)^(b-1) dt,}
  and 6.6.2 \eqn{I_x(a,b) = B_x(a,b) / B(a,b)} where
  \eqn{B(a,b) = B_1(a,b)} is the Beta function (\code{\link{beta}}).

  \eqn{I_x(a,b)} is \code{pbeta(x,a,b)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.
}
\seealso{
  \code{\link{beta}} for the Beta function, and \code{\link{dgamma}} for
  the Gamma distribution.
}
\examples{
x <- seq(0, 1, length=21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)
}
\keyword{distribution}
