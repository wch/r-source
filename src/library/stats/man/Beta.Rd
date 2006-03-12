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
dbeta(x, shape1, shape2, ncp = 0, log = FALSE)
pbeta(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
qbeta(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
rbeta(n, shape1, shape2, ncp = 0)
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

  The non-central Beta distribution is defined (Johnson et al, 1995,
  pp. 502) as the distribution of \eqn{X/(X+Y)} where
  \eqn{X \sim \chi^2_{2a}(\lambda)}{X ~ chi^2_2a(lambda)} and
  \eqn{Y \sim \chi^2_{2b}}{Y ~ chi^2_2b}.
}
\value{
  \code{dbeta} gives the density, \code{pbeta} the distribution
  function, \code{qbeta} the quantile function, and \code{rbeta}
  generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.
}
\source{
  The central \code{dbeta} is based on a binomial probability, using code
  contributed by Catherine Loader (see \code{\link{dbinom}}) if either
  shape parameter is larger than one, otherwise directly from the definition.
  The non-central case is based on the derivation as a Poisson
  mixture of betas (Johnson \emph{et al}, 1995, pp. 502--3).

  The central \code{pbeta} uses a C translation of

  Didonato, A. and Morris, A., Jr, (1992)
  Algorithm 708: Significant digit computation of the incomplete beta
  function ratios,
  \emph{ACM Transactions on Mathematical Software}, \bold{18}, 360--373.
  (See also\crBrown, B. and Lawrence Levy, L. (1994)
  Certification of algorithm 708: Significant digit computation of the
  incomplete beta,
  \emph{ACM Transactions on Mathematical Software}, \bold{20}, 393--397.)

  The non-central \code{pbeta} uses a C translation of
  
  Lenth, R. V. (1987) Algorithm AS226: Computing noncentral beta
  probabilities. \emph{Appl. Statist}, \bold{36}, 241--244,\cr
incorporating AS R84 (1990), \emph{Appl. Statist}, \bold{39}, 311--2.
  
  \code{qbeta} is based on a C translation of

  Cran, G. W., K. J. Martin and G. E. Thomas (1977).
  Remark AS R19 and Algorithm AS 109,
  \emph{Applied Statistics},  \bold{26}, 111--114,
  and subsequent remarks (AS83 and correction).

  \code{rbeta} is based on a C translation of

  R. C. H. Cheng (1978).
  Generating beta variates with nonintegral shape parameters.
  \emph{Communications of the ACM}, \bold{21}, 317--322.  
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 2, especially
  chapter 25. Wiley, New York.
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
