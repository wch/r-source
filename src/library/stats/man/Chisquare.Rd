% File src/library/stats/man/Chisquare.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Chisquare}
\alias{Chisquare}
\alias{dchisq}
\alias{pchisq}
\alias{qchisq}
\alias{rchisq}
\title{The (non-central) Chi-Squared Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the chi-squared (\eqn{\chi^2}{chi^2}) distribution with
  \code{df} degrees of freedom and optional non-centrality parameter
  \code{ncp}.
}
\usage{
dchisq(x, df, ncp = 0, log = FALSE)
pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
rchisq(n, df, ncp = 0)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{df}{degrees of freedom (non-negative, but can be non-integer).}
  \item{ncp}{non-centrality parameter (non-negative).}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dchisq} gives the density, \code{pchisq} gives the distribution
  function, \code{qchisq} gives the quantile function, and \code{rchisq}
  generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rchisq}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}
\details{
  The chi-squared distribution with \code{df}\eqn{= n \ge 0}
  degrees of freedom has density
  \deqn{f_n(x) = \frac{1}{{2}^{n/2} \Gamma (n/2)} {x}^{n/2-1} {e}^{-x/2}}{f_n(x) = 1 / (2^(n/2) \Gamma(n/2))  x^(n/2-1) e^(-x/2)}
  for \eqn{x > 0}.  The mean and variance are \eqn{n} and \eqn{2n}.

  The non-central chi-squared distribution with \code{df}\eqn{= n}
  degrees of freedom and non-centrality parameter \code{ncp}
  \eqn{= \lambda} has density
  \deqn{
    f(x) = e^{-\lambda / 2}
      \sum_{r=0}^\infty \frac{(\lambda/2)^r}{r!}\, f_{n + 2r}(x)}{f(x) = exp(-\lambda/2) SUM_{r=0}^\infty ((\lambda/2)^r / r!) dchisq(x, df + 2r)
  }
  for \eqn{x \ge 0}.  For integer \eqn{n}, this is the distribution of
  the sum of squares of \eqn{n} normals each with variance one,
  \eqn{\lambda} being the sum of squares of the normal means; further,
  \cr
  \eqn{E(X) = n + \lambda}, \eqn{Var(X) = 2(n + 2*\lambda)}, and
  \eqn{E((X - E(X))^3) = 8(n + 3*\lambda)}.

  Note that the degrees of freedom \code{df}\eqn{= n}, can be
  non-integer, and also \eqn{n = 0} which is relevant for
  non-centrality \eqn{\lambda > 0},
  see Johnson \emph{et al} (1995, chapter 29).

  Note that \code{ncp} values larger than about 1e5 may give inaccurate
  results with many warnings for \code{pchisq} and \code{qchisq}.
}
\source{
  The central cases are computed via the gamma distribution.

  The non-central \code{dchisq} and \code{rchisq} are computed as a
  Poisson mixture central of chi-squares (Johnson \emph{et al}, 1995, p.436).

  The non-central \code{pchisq} is for \code{ncp < 80} computed from
  the Poisson mixture of central chi-squares and for larger \code{ncp}
  \emph{via} a C translation of

  Ding, C. G. (1992)
  Algorithm AS275: Computing the non-central chi-squared
  distribution function. \emph{Appl.Statist.}, \bold{41} 478--482.

  which computes the lower tail only (so the upper tail suffers from
  cancellation and a warning will be given when this is likely to be
  significant).

  The non-central \code{qchisq} is based on inversion of \code{pchisq}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, chapters 18 (volume 1)
  and 29 (volume 2). Wiley, New York.
}
\note{
  Supplying \code{ncp = 0} uses the algorithm for the non-central
  distribution, which is not the same algorithm used if \code{ncp} is
  omitted.  This is to give consistent behaviour in extreme cases with
  values of \code{ncp} very near zero.

  The code for non-zero \code{ncp} is principally intended to be used
  for moderate values of \code{ncp}: it will not be highly accurate,
  especially in the tails, for large values.
}
\seealso{
  \link{Distributions} for other standard distributions.

  A central chi-squared distribution with \eqn{n} degrees of freedom
  is the same as a Gamma distribution with \code{shape} \eqn{\alpha =
    n/2}{a = n/2} and \code{scale} \eqn{\sigma = 2}{s = 2}.  Hence, see
  \code{\link{dgamma}} for the Gamma distribution.
}
\examples{
require(graphics)

dchisq(1, df = 1:3)
pchisq(1, df =  3)
pchisq(1, df =  3, ncp = 0:4)  # includes the above

x <- 1:10
## Chi-squared(df = 2) is a special exponential distribution
all.equal(dchisq(x, df = 2), dexp(x, 1/2))
all.equal(pchisq(x, df = 2), pexp(x, 1/2))

## non-central RNG -- df = 0 with ncp > 0:  Z0 has point mass at 0!
Z0 <- rchisq(100, df = 0, ncp = 2.)
graphics::stem(Z0)

\donttest{## visual testing
## do P-P plots for 1000 points at various degrees of freedom
L <- 1.2; n <- 1000; pp <- ppoints(n)
op <- par(mfrow = c(3,3), mar = c(3,3,1,1)+.1, mgp = c(1.5,.6,0),
          oma = c(0,0,3,0))
for(df in 2^(4*rnorm(9))) {
  plot(pp, sort(pchisq(rr <- rchisq(n, df = df, ncp = L), df = df, ncp = L)),
       ylab = "pchisq(rchisq(.),.)", pch = ".")
  mtext(paste("df = ", formatC(df, digits = 4)), line =  -2, adj = 0.05)
  abline(0, 1, col = 2)
}
mtext(expression("P-P plots : Noncentral  "*
                 chi^2 *"(n=1000, df=X, ncp= 1.2)"),
      cex = 1.5, font = 2, outer = TRUE)
par(op)}

## "analytical" test
lam <- seq(0, 100, by = .25)
p00 <- pchisq(0,      df = 0, ncp = lam)
p.0 <- pchisq(1e-300, df = 0, ncp = lam)
stopifnot(all.equal(p00, exp(-lam/2)),
          all.equal(p.0, exp(-lam/2)))
}
\keyword{distribution}
