\name{TDist}
\alias{TDist}
\alias{dt}
\alias{pt}
\alias{qt}
\alias{rt}
\title{The Student t Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the t distribution with \code{df} degrees of freedom
  (and optional noncentrality parameter \code{ncp}).
}
\usage{
dt(x, df, ncp=0, log = FALSE)
pt(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE)
qt(p, df,        lower.tail = TRUE, log.p = FALSE)
rt(n, df)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{df}{degrees of freedom (\eqn{> 0}, maybe non-integer).}
  \item{ncp}{non-centrality parameter \eqn{\delta};
    currently for \code{pt()} and \code{dt()}, only for \code{ncp <= 37.62}.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{dt} gives the density,
  \code{pt} gives the distribution function,
  \code{qt} gives the quantile function, and
  \code{rt} generates random deviates.
}
\details{
  The \eqn{t} distribution with \code{df} \eqn{= \nu}{= n} degrees of
  freedom has density
  \deqn{
    f(x) = \frac{\Gamma ((\nu+1)/2)}{\sqrt{\pi \nu} \Gamma (\nu/2)}
    (1 + x^2/\nu)^{-(\nu+1)/2}%
  }{f(x) = Gamma((n+1)/2) / (sqrt(n pi) Gamma(n/2)) (1 + x^2/n)^-((n+1)/2)}
  for all real \eqn{x}.
  It has mean \eqn{0} (for \eqn{\nu > 1}{n > 1}) and
  variance \eqn{\frac{\nu}{\nu-2}}{n/(n-2)} (for \eqn{\nu > 2}{n > 2}).

  The general \emph{non-central} \eqn{t}
  with parameters \eqn{(\nu,\delta)}{(df,Del)} \code{= (df, ncp)}
  is defined as the distribution of
  \eqn{T_{\nu}(\delta) := \frac{U + \delta}{\chi_{\nu}/\sqrt{\nu}}}{%
      T(df, Del) := (U + Del) / (Chi(df) / sqrt(df)) }
  where \eqn{U} and \eqn{\chi_{\nu}}{Chi(df)}  are independent random
  variables, \eqn{U \sim {\cal N}(0,1)}{U \~ N(0,1)}, and
%%fails  \eqn{{\chi_{\nu}}^2}{(Chi(df))^2}
  \eqn{\chi^2_\nu}{Chi(df)^2}
  is chi-squared, see \code{\link{pchisq}}.

  The most used applications are power calculations for \eqn{t}-tests:\cr
  Let \eqn{T= \frac{\bar{X} - \mu_0}{S/\sqrt{n}}}{T= (mX - m0) / (S/sqrt(n))}
  where
  \eqn{\bar{X}}{mX} is the \code{\link{mean}} and \eqn{S} the sample standard
  deviation (\code{\link{sd}}) of \eqn{X_1,X_2,\dots,X_n} which are i.i.d.
%%fails \eqn{{\cal N}(\mu,\sigma^2)}{N(mu,sigma^2)}
        \eqn{       N(\mu,\sigma^2)}{N(mu,sigma^2)}.
  Then \eqn{T} is distributed as non-centrally \eqn{t} with
  \code{df}\eqn{= n-1}
  degrees of freedom and \bold{n}on-\bold{c}entrality \bold{p}arameter
  \code{ncp}\eqn{= (\mu - \mu_0) \sqrt{n}/\sigma}{= (mu - m0) * sqrt(n)/sigma}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (except non-central versions.)

  Lenth, R. V. (1989). \emph{Algorithm AS 243} ---
    Cumulative distribution function of the non-central \eqn{t} distribution,
    \emph{Appl.\ Statist.} \bold{38}, 185--189.
}
\seealso{\code{\link{df}} for the F distribution.}
\examples{
1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

tt <- seq(0,10, len=21)
ncp <- seq(0,6, len=31)
ptn <- outer(tt,ncp, function(t,d) pt(t, df = 3, ncp=d))
image(tt,ncp,ptn, zlim=c(0,1),main=t.tit <- "Non-central t - Probabilities")
persp(tt,ncp,ptn, zlim=0:1, r=2, phi=20, theta=200, main=t.tit,
      xlab = "t", ylab = "noncentrality parameter", zlab = "Pr(T <= t)")

op <- par(yaxs="i")
plot(function(x) dt(x, df = 3, ncp = 2), -3, 11, ylim = c(0, 0.32),
     main="Non-central t - Density")
par(op)
}
\keyword{distribution}
