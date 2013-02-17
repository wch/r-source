% File src/library/stats/man/TDist.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{TDist}
\encoding{UTF-8}
\alias{TDist}
\alias{dt}
\alias{pt}
\alias{qt}
\alias{rt}
\title{The Student t Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the t distribution with \code{df} degrees of freedom
  (and optional non-centrality parameter \code{ncp}).
}
\usage{
dt(x, df, ncp, log = FALSE)
pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rt(n, df, ncp)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{df}{degrees of freedom (\eqn{> 0}, maybe non-integer).  \code{df
      = Inf} is allowed.}
  \item{ncp}{non-centrality parameter \eqn{\delta}{delta};
    currently except for \code{rt()}, only for \code{abs(ncp) <= 37.62}.
    If omitted, use the central t distribution.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dt} gives the density,
  \code{pt} gives the distribution function,
  \code{qt} gives the quantile function, and
  \code{rt} generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.

  The length of the result is determined by \code{n} for
  \code{rt}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
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
\details{
  The \eqn{t} distribution with \code{df} \eqn{= \nu}{= n} degrees of
  freedom has density
  \deqn{
    f(x) = \frac{\Gamma ((\nu+1)/2)}{\sqrt{\pi \nu} \Gamma (\nu/2)}
    (1 + x^2/\nu)^{-(\nu+1)/2}%
  }{f(x) = \Gamma((n+1)/2) / (\sqrt(n \pi) \Gamma(n/2)) (1 + x^2/n)^-((n+1)/2)}
  for all real \eqn{x}.
  It has mean \eqn{0} (for \eqn{\nu > 1}{n > 1}) and
  variance \eqn{\frac{\nu}{\nu-2}}{n/(n-2)} (for \eqn{\nu > 2}{n > 2}).

  The general \emph{non-central} \eqn{t}
  with parameters \eqn{(\nu, \delta)}{(df, Del)} \code{= (df, ncp)}
  is defined as the distribution of
  \eqn{T_{\nu}(\delta) := (U + \delta)/\sqrt{V/\nu}}{T(df, Del) := (U + Del) / \sqrt(V/df) }
  where \eqn{U} and \eqn{V}  are independent random
  variables, \eqn{U \sim {\cal N}(0,1)}{U ~ N(0,1)} and
  \eqn{V \sim \chi^2_\nu}{V ~ \chi^2(df)} (see \link{Chisquare}).

  The most used applications are power calculations for \eqn{t}-tests:\cr
  Let \eqn{T = \frac{\bar{X} - \mu_0}{S/\sqrt{n}}}{T= (mX - m0) / (S/sqrt(n))}
  where
  \eqn{\bar{X}}{mX} is the \code{\link{mean}} and \eqn{S} the sample standard
  deviation (\code{\link{sd}}) of \eqn{X_1, X_2, \dots, X_n} which are
  i.i.d. \eqn{{\cal N}(\mu, \sigma^2)}{N(\mu, \sigma^2)}
  Then \eqn{T} is distributed as non-central \eqn{t} with
  \code{df}\eqn{{} = n-1}{= n - 1}
  degrees of freedom and \bold{n}on-\bold{c}entrality \bold{p}arameter
  \code{ncp}\eqn{{} = (\mu - \mu_0) \sqrt{n}/\sigma}{ = (\mu - m0) * sqrt(n)/\sigma}.
}
\source{
  The central \code{dt} is computed via an accurate formula
  provided by Catherine Loader (see the reference in \code{\link{dbinom}}).

  For the non-central case of \code{dt}, C code contributed by
  Claus \enc{Ekstrøm}{Ekstroem} based on the relationship (for
  \eqn{x \neq 0}{x != 0}) to the cumulative distribution.

  For the central case of \code{pt}, a normal approximation in the
  tails, otherwise via \code{\link{pbeta}}.

  For the non-central case of \code{pt} based on a C translation of

  Lenth, R. V. (1989). \emph{Algorithm AS 243} ---
  Cumulative distribution function of the non-central \eqn{t} distribution,
  \emph{Applied Statistics} \bold{38}, 185--189.

  This computes the lower tail only, so the upper tail suffers from
  cancellation and a warning will be given when this is likely to be
  significant.

  For central \code{qt}, a C translation of

  Hill, G. W. (1970) Algorithm 396: Student's t-quantiles.
  \emph{Communications of the ACM}, \bold{13(10)}, 619--620.

  altered to take account of

  Hill, G. W. (1981) Remark on Algorithm 396, \emph{ACM Transactions on
    Mathematical Software}, \bold{7}, 250--1.

  The non-central case is done by inversion.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole. (Except non-central versions.)

  Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995)
  \emph{Continuous Univariate Distributions}, volume 2, chapters 28 and 31.
  Wiley, New York.
}
\seealso{
  \link{Distributions} for other standard distributions, including
  \code{\link{df}} for the F distribution.
}
\examples{
require(graphics)

1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

tt <- seq(0, 10, len = 21)
ncp <- seq(0, 6, len = 31)
ptn <- outer(tt, ncp, function(t, d) pt(t, df = 3, ncp = d))
t.tit <- "Non-central t - Probabilities"
image(tt, ncp, ptn, zlim = c(0,1), main = t.tit)
persp(tt, ncp, ptn, zlim = 0:1, r = 2, phi = 20, theta = 200, main = t.tit,
      xlab = "t", ylab = "non-centrality parameter",
      zlab = "Pr(T <= t)")

plot(function(x) dt(x, df = 3, ncp = 2), -3, 11, ylim = c(0, 0.32),
     main = "Non-central t - Density", yaxs = "i")
}
\keyword{distribution}
