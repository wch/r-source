\name{bandwidth}
\alias{bw.nrd0}
\alias{bw.nrd}
\alias{bw.ucv}
\alias{bw.bcv}
\alias{bw.SJ}
\concept{bandwidth}
\title{Bandwidth Selectors for Kernel Density Estimation}
\usage{
bw.nrd0(x)

bw.nrd(x)

bw.ucv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax)

bw.bcv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax)

bw.SJ(x, nb = 1000, lower = 0.1 * hmax, upper = hmax,
      method = c("ste", "dpi"))
}
\arguments{
  \item{x}{A data vector.}
  \item{nb}{number of bins to use.}
  \item{lower, upper}{Range over which to minimize.  The default is
    almost always satisfactory.  \code{hmax} is calculated internally
    from a normal reference bandwidth.}
  \item{method}{Either \code{"ste"} ("solve-the-equation") or
    \code{"dpi"} ("direct plug-in").}
}
\description{
  Bandwidth selectors for gaussian windows in \code{\link{density}}.
}

\details{
  \code{bw.nrd0} implements a rule-of-thumb for
  choosing the bandwidth of a Gaussian kernel density estimator.
  It defaults to 0.9 times the
  minimum of the standard deviation and the interquartile range divided by
  1.34 times the sample size to the negative one-fifth power
  (= Silverman's \dQuote{rule of thumb}, Silverman (1986, page 48, eqn (3.31))
  \emph{unless} the quartiles coincide when a positive result
  will be guaranteed.

  \code{bw.nrd} is the more common variation given by Scott (1992),
  using factor 1.06.

  \code{bw.ucv} and \code{bw.bcv} implement unbiased and
  biased cross-validation respectively.

  \code{bw.SJ} implements the methods of Sheather & Jones (1991)
  to select the bandwidth using pilot estimation of derivatives.\cr
  The algorithm solves an equation (via \code{\link{uniroot}}) and
  because of that, enlarges the interval \code{c(lower,upper)} when
  the boundaries were not user-specified and do not bracket the root.
}
\value{
  A bandwidth on a scale suitable for the \code{bw} argument
  of \code{density}.
}
\seealso{
  \code{\link{density}}.

  \code{\link[MASS]{bandwidth.nrd}}, \code{\link[MASS]{ucv}},
  \code{\link[MASS]{bcv}} and \code{\link[MASS]{width.SJ}} in
  package \pkg{MASS}, which are all scaled to the \code{width} argument
  of \code{density} and so give answers four times as large.
}
\references{
  Scott, D. W. (1992)
  \emph{Multivariate Density Estimation: Theory, Practice, and
    Visualization.}
  Wiley.

  Sheather, S. J. and Jones, M. C. (1991)
  A reliable data-based bandwidth selection method for kernel density
  estimation.
  \emph{Journal of the Royal Statistical Society series B},
  \bold{53}, 683--690.

  Silverman, B. W. (1986)
  \emph{Density Estimation}.
  London: Chapman and Hall.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S}.
  Springer.
}
\examples{
require(graphics)

plot(density(precip, n = 1000))
rug(precip)
lines(density(precip, bw="nrd"), col = 2)
lines(density(precip, bw="ucv"), col = 3)
lines(density(precip, bw="bcv"), col = 4)
lines(density(precip, bw="SJ-ste"), col = 5)
lines(density(precip, bw="SJ-dpi"), col = 6)
legend(55, 0.035,
       legend = c("nrd0", "nrd", "ucv", "bcv", "SJ-ste", "SJ-dpi"),
       col = 1:6, lty = 1)
}
\keyword{distribution}
\keyword{smooth}
