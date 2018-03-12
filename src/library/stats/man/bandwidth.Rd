% File src/library/stats/man/bandwidth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

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

bw.ucv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax,
       tol = 0.1 * lower)

bw.bcv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax,
       tol = 0.1 * lower)

bw.SJ(x, nb = 1000, lower = 0.1 * hmax, upper = hmax,
      method = c("ste", "dpi"), tol = 0.1 * lower)
}
\arguments{
  \item{x}{numeric vector.}
  \item{nb}{number of bins to use.}
  \item{lower, upper}{range over which to minimize.  The default is
    almost always satisfactory.  \code{hmax} is calculated internally
    from a normal reference bandwidth.}
  \item{method}{either \code{"ste"} ("solve-the-equation") or
    \code{"dpi"} ("direct plug-in").   Can be abbreviated.}
  \item{tol}{for method \code{"ste"}, the convergence tolerance for
    \code{\link{uniroot}}.  The default leads to bandwidth estimates
    with only slightly more than one digit accuracy, which is sufficient
    for practical density estimation, but possibly not for theoretical
    simulation studies.}
}
\description{
  Bandwidth selectors for Gaussian kernels in \code{\link{density}}.
}

\details{
  \code{bw.nrd0} implements a rule-of-thumb for
  choosing the bandwidth of a Gaussian kernel density estimator.
  It defaults to 0.9 times the
  minimum of the standard deviation and the interquartile range divided by
  1.34 times the sample size to the negative one-fifth power
  (= Silverman's \sQuote{rule of thumb}, Silverman (1986, page 48, eqn (3.31)))
  \emph{unless} the quartiles coincide when a positive result
  will be guaranteed.

  \code{bw.nrd} is the more common variation given by Scott (1992),
  using factor 1.06.

  \code{bw.ucv} and \code{bw.bcv} implement unbiased and
  biased cross-validation respectively.

  \code{bw.SJ} implements the methods of Sheather & Jones (1991)
  to select the bandwidth using pilot estimation of derivatives.\cr
  The algorithm for method \code{"ste"} solves an equation (via
  \code{\link{uniroot}}) and because of that, enlarges the interval
  \code{c(lower, upper)} when the boundaries were not user-specified and
  do not bracket the root.

  The last three methods use all pairwise binned distances: they are of
  complexity \eqn{O(n^2)} up to \code{n = nb/2} and \eqn{O(n)}
  thereafter.  Because of the binning, the results differ slightly when
  \code{x} is translated or sign-flipped.
}
\value{
  A bandwidth on a scale suitable for the \code{bw} argument
  of \code{density}.
}
\note{
  Long vectors \code{x} are not supported, but neither are they by
  \code{\link{density}} and kernel density estimation and for more than
  a few thousand points a histogram would be preferred.
}
\author{
  B. D. Ripley, taken from early versions of package \pkg{MASS}.
}
\seealso{
  \code{\link{density}}.

  \code{\link[MASS]{bandwidth.nrd}}, \code{\link[MASS]{ucv}},
  \code{\link[MASS]{bcv}} and \code{\link[MASS]{width.SJ}} in
  package \CRANpkg{MASS}, which are all scaled to the \code{width} argument
  of \code{density} and so give answers four times as large.
}
\references{
  Scott, D. W. (1992)
  \emph{Multivariate Density Estimation: Theory, Practice, and
    Visualization.}
  New York: Wiley.

  Sheather, S. J. and Jones, M. C. (1991).
  A reliable data-based bandwidth selection method for kernel density
  estimation.
  \emph{Journal of the Royal Statistical Society series B},
  \bold{53}, 683--690.
  \url{http://www.jstor.org/stable/2345597}.

  Silverman, B. W. (1986).
  \emph{Density Estimation}.
  London: Chapman and Hall.

  Venables, W. N. and Ripley, B. D. (2002).
  \emph{Modern Applied Statistics with S}.
  Springer.
}
\examples{
require(graphics)

plot(density(precip, n = 1000))
rug(precip)
lines(density(precip, bw = "nrd"), col = 2)
lines(density(precip, bw = "ucv"), col = 3)
lines(density(precip, bw = "bcv"), col = 4)
lines(density(precip, bw = "SJ-ste"), col = 5)
lines(density(precip, bw = "SJ-dpi"), col = 6)
legend(55, 0.035,
       legend = c("nrd0", "nrd", "ucv", "bcv", "SJ-ste", "SJ-dpi"),
       col = 1:6, lty = 1)
}
\keyword{distribution}
\keyword{smooth}
