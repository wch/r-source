% File src/library/stats/man/spectrum.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{spectrum}
\alias{spectrum}
\alias{spec}
\title{Spectral Density Estimation}
\description{
  The \code{spectrum} function estimates the spectral density of a
  time series.
}
\usage{
spectrum(x, \dots, method = c("pgram", "ar"))
}
\arguments{
  \item{x}{A univariate or multivariate time series.}
  \item{method}{String specifying the method used to estimate the
    spectral density.  Allowed methods are \code{"pgram"} (the default)
    and \code{"ar"}.}
  \item{\dots}{Further arguments to specific spec methods or
    \code{plot.spec}.}
}
\value{
  An object of class \code{"spec"}, which is a list containing at
  least the following components:
  \item{freq}{vector of frequencies at which the spectral
    density is estimated. (Possibly approximate Fourier frequencies.)
    The units are the reciprocal of cycles per unit time (and not per
    observation spacing): see \sQuote{Details} below.}

  \item{spec}{Vector (for univariate series) or matrix (for multivariate
    series) of estimates of the spectral density at frequencies
    corresponding to \code{freq}.}

  \item{coh}{\code{NULL} for univariate series. For multivariate time
    series, a matrix containing the \emph{squared} coherency
    between different
    series. Column \eqn{ i + (j - 1) * (j - 2)/2} of \code{coh}
    contains the squared coherency between columns \eqn{i} and \eqn{j}
    of \code{x}, where \eqn{i < j}.}

  \item{phase}{\code{NULL} for univariate series. For multivariate
    time series a matrix containing the cross-spectrum phase between
    different series. The format is the same as \code{coh}.}

  \item{series}{The name of the time series.}

  \item{snames}{For multivariate input, the names of the component series.}

  \item{method}{The method used to calculate the spectrum.}

  The result is returned invisibly if \code{plot} is true.
}
\details{
  \code{spectrum} is a wrapper function which calls the methods
  \code{\link{spec.pgram}} and \code{\link{spec.ar}}.

  The spectrum here is defined with scaling \code{1/\link{frequency}(x)},
  following S-PLUS.  This makes the spectral density a density over the
  range \code{(-frequency(x)/2, +frequency(x)/2]}, whereas a more common
  scaling is \eqn{2\pi}{2pi} and range \eqn{(-0.5, 0.5]} (e.g., Bloomfield)
  or 1 and range \eqn{(-\pi, \pi]}{(-pi, pi]}.

  If available, a confidence interval will be plotted by
  \code{plot.spec}: this is asymmetric, and the width of the centre
  mark indicates the equivalent bandwidth.
}
\note{
  The default plot for objects of class \code{"spec"} is quite complex,
  including an error bar and default title, subtitle and axis
  labels.  The defaults can all be overridden by supplying the
  appropriate graphical parameters.
}
\references{
  Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
    Introduction.} Wiley.

  Brockwell, P. J. and Davis, R. A. (1991) \emph{Time Series: Theory and
    Methods.} Second edition. Springer.

  Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied
    Statistics with S-PLUS.} Fourth edition. Springer. (Especially
  pages 392--7.)
}
\author{Martyn Plummer, B.D. Ripley}
\seealso{
  \code{\link{spec.ar}},
  \code{\link{spec.pgram}};
  \code{\link{plot.spec}}.
}
\examples{
require(graphics)

## Examples from Venables & Ripley
## spec.pgram
par(mfrow = c(2,2))
spectrum(lh)
spectrum(lh, spans = 3)
spectrum(lh, spans = c(3,3))
spectrum(lh, spans = c(3,5))

spectrum(ldeaths)
spectrum(ldeaths, spans = c(3,3))
spectrum(ldeaths, spans = c(3,5))
spectrum(ldeaths, spans = c(5,7))
spectrum(ldeaths, spans = c(5,7), log = "dB", ci = 0.8)

# for multivariate examples see the help for spec.pgram

## spec.ar
spectrum(lh, method = "ar")
spectrum(ldeaths, method = "ar")
}
\keyword{ts}
