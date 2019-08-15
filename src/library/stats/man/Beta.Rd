% File src/library/stats/man/Beta.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

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
  \item{shape1, shape2}{non-negative parameters of the Beta distribution.}
  \item{ncp}{non-centrality parameter.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\details{
  The Beta distribution with parameters \code{shape1} \eqn{= a} and
  \code{shape2} \eqn{= b} has density
  \deqn{f(x)=\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}{x}^{a-1} {(1-x)}^{b-1}%
  }{\Gamma(a+b)/(\Gamma(a)\Gamma(b))x^(a-1)(1-x)^(b-1)}
  for \eqn{a > 0}, \eqn{b > 0} and \eqn{0 \le x \le 1}
  where the boundary values at \eqn{x=0} or \eqn{x=1} are defined as
  by continuity (as limits).
  \cr
  The mean is \eqn{a/(a+b)} and the variance is \eqn{ab/((a+b)^2 (a+b+1))}.
  These moments and all distributional properties can be defined as
  limits (leading to point masses at 0, 1/2, or 1) when \eqn{a} or
  \eqn{b} are zero or infinite, and the corresponding
  \code{[dpqr]beta()} functions are defined correspondingly.

  \code{pbeta} is closely related to the incomplete beta function.  As
  defined by Abramowitz and Stegun 6.6.1
  \deqn{B_x(a,b) = \int_0^x t^{a-1} (1-t)^{b-1} dt,}{B_x(a,b) =
    integral_0^x t^(a-1) (1-t)^(b-1) dt,}
  and 6.6.2 \eqn{I_x(a,b) = B_x(a,b) / B(a,b)} where
  \eqn{B(a,b) = B_1(a,b)} is the Beta function (\code{\link{beta}}).

  \eqn{I_x(a,b)} is \code{pbeta(x, a, b)}.

  The noncentral Beta distribution (with \code{ncp} \eqn{ = \lambda})
  is defined (Johnson \emph{et al}, 1995, pp.\sspace{}502) as the distribution of
  \eqn{X/(X+Y)} where \eqn{X \sim \chi^2_{2a}(\lambda)}{X ~ chi^2_2a(\lambda)}
  and \eqn{Y \sim \chi^2_{2b}}{Y ~ chi^2_2b}.
}
\value{
  \code{dbeta} gives the density, \code{pbeta} the distribution
  function, \code{qbeta} the quantile function, and \code{rbeta}
  generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rbeta}, and is the maximum of the lengths of the
  numerical arguments for the other functions.

  The numerical arguments other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  arguments are used.
}
\note{
  Supplying \code{ncp = 0} uses the algorithm for the non-central
  distribution, which is not the same algorithm as when \code{ncp} is
  omitted.  This is to give consistent behaviour in extreme cases with
  values of \code{ncp} very near zero.
}
\source{
 \itemize{
  \item The central \code{dbeta} is based on a binomial probability, using code
  contributed by Catherine Loader (see \code{\link{dbinom}}) if either
  shape parameter is larger than one, otherwise directly from the definition.
  The non-central case is based on the derivation as a Poisson
  mixture of betas (Johnson \emph{et al}, 1995, pp.\sspace{}502--3).

  \item The central \code{pbeta} for the default (\code{log_p = FALSE})
  uses a C translation based on

  Didonato, A. and Morris, A., Jr, (1992)
  Algorithm 708: Significant digit computation of the incomplete beta
  function ratios,
  \emph{ACM Transactions on Mathematical Software}, \bold{18}, 360--373.
  (See also\cr
  Brown, B. and Lawrence Levy, L. (1994)
  Certification of algorithm 708: Significant digit computation of the
  incomplete beta,
  \emph{ACM Transactions on Mathematical Software}, \bold{20}, 393--397.)
  \cr %%
  We have slightly tweaked the original \dQuote{TOMS 708} algorithm, and
  enhanced for \code{log.p = TRUE}.  For that (log-scale) case,
  underflow to \code{-Inf} (i.e., \eqn{P = 0}) or \code{0}, (i.e.,
  \eqn{P = 1}) still happens because the original algorithm was designed
  without log-scale considerations.  Underflow to \code{-Inf} now
  typically signals a \code{\link{warning}}.

  \item The non-central \code{pbeta} uses a C translation of

  Lenth, R. V. (1987) Algorithm AS226: Computing noncentral beta
  probabilities. \emph{Appl. Statist}, \bold{36}, 241--244,
  incorporating\cr
  Frick, H. (1990)'s AS R84, \emph{Appl. Statist}, \bold{39}, 311--2,
  and\cr
  Lam, M.L. (1995)'s AS R95, \emph{Appl. Statist}, \bold{44}, 551--2.

  This computes the lower tail only, so the upper tail suffers from
  cancellation and a warning will be given when this is likely to be
  significant.

  \item The central case of \code{qbeta} is based on a C translation of

  Cran, G. W., K. J. Martin and G. E. Thomas (1977).
  Remark AS R19 and Algorithm AS 109,
  \emph{Applied Statistics},  \bold{26}, 111--114,
  and subsequent remarks (AS83 and correction).

  \item  The central case of \code{rbeta} is based on a C translation of

  R. C. H. Cheng (1978).
  Generating beta variates with nonintegral shape parameters.
  \emph{Communications of the ACM}, \bold{21}, 317--322.
 }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 2, especially
  chapter 25. Wiley, New York.
}
\seealso{
  \link{Distributions} for other standard distributions.

  \code{\link{beta}} for the Beta function.
}
\examples{
x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)

## Visualization, including limit cases:
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=\%g, b=\%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}
pl.beta(3,1)

pl.beta(2, 4)
pl.beta(3, 7)
pl.beta(3, 7, asp=1)

pl.beta(0, 0)   ## point masses at  {0, 1}

pl.beta(0, 2)   ## point mass at 0 ; the same as
pl.beta(1, Inf)

pl.beta(Inf, 2) ## point mass at 1 ; the same as
pl.beta(3, 0)

pl.beta(Inf, Inf)# point mass at 1/2
}
\keyword{distribution}
