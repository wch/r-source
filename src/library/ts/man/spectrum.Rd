\name{spectrum}
\alias{spectrum}
\alias{plot.spec}
\alias{plot.spec.coherency}
\alias{plot.spec.phase}
\alias{spec}
\title{Spectral Density Estimation}
\usage{
spectrum(x, method = c("pgram","ar"), plot = TRUE, ...)
plot.spec(spec.obj, add=FALSE, ci=0.95,
          log = c("yes", "dB", "no"), ci.col = "blue", ci.lty = 3, 
          plot.type = c("marginal", "coherency", "phase"), ...)
}
\arguments{
  \item{x}{A univariate or multivariate time series.}

  \item{method}{String specifying the method used to estimate the spectral 
    density. Allowed methods are "pgram" (the default) and "ar".}

  \item{plot}{logical. If \code{TRUE} then the spectral density is plotted.}

  \item{\dots}{Further arguments to specific spec methods or
    \code{plot.spec}.}

  \item{spec.obj}{An object of class \code{spec}.}

  \item{add}{logical. If \code{TRUE} then lines are added to the
    existing plot.}

  \item{ci}{Coverage probability for confidence interval. Plotting of the
    confidence bar is omitted unless \code{ci} is strictly positive.}

  \item{log}{If \code{"dB"}, plot on log10 (decibel) scale (as S-PLUS),
    otherwise use conventional log scale or linear scale. Logical values
    are also accepted.  The default is \code{"yes"} unless
    \code{options(ts.S.compat = TRUE)} has been set, when it is
    \code{"dB"}.}

  \item{ci.col, ci.lty}{Colour for plotting confidence bar, colour and
    line type for confidence intervals for coherency and phase.}

  \item{plot.type}{For multivariate time series, the type of plot
    required. Only the first character is needed.}

  \item{\dots}{Further graphical parameters.}
}
\description{
  The \code{spectrum} function estimates the spectral density of a
  time series. This is a wrapper function which calls the methods
  \code{\link{spec.pgram}} and \code{\link{spec.ar}}.

  The generic function \code{plot} has a method for \code{spec} objects:
  for multivariate time series it plots the marginal spectra of the
  series or pairs plots of the coherency and phase of the cross-spectra.
}
\value{
  An object of class \code{spec}, which is a list containing at
  least the following elements:
  \item{freq}{vector of frequencies at which the spectral
    density is estimated. (Possibly approximate Fourier frequencies.)}

  \item{spec}{Vector (for univariate series) or matrix (for multivariate
    series) of estimates of the spectral density at frequencies
    corresponding to \code{freq}.}

  \item{coh}{\code{NULL} for univariate series. For multivariate time
    series, a matrix containing the \emph{squared} coherency
    between different
    series. Column \eqn{ i + (j - 1) * (j - 2)/2} of \code{coh}
    contains the squared coherency between columns \eqn{i} and \eqn{j}
    of \code{x}, where \eqn{i > j}.}

  \item{phase}{\code{NULL} for univariate series. For multivariate
    time series a matrix containing the cross-spectrum phase between
    different series. The format is the same as \code{coh}.}

  \item{series}{The name of the time series.}

  \item{snames}{For multivariate input, the names of the component series.}

  \item{method}{The method used to calculate the spectrum.}

  The result is returned invisibly if \code{plot} is true.
}
\details{
  The spectrum here is defined with scaling \code{1/frequency(x)},
  following S-PLUS. This makes the spectral density a density over the
  range \code{(-frequency(x)/2, +frequency(x)/2]}, whereas a more
  common scaling is \eqn{2\pi} and range  \eqn{(-0.5, 0.5]}
  (e.g. Bloomfield) or 1 and range \eqn{(-\pi, \pi]}.

  If available, a confidence interval will be plotted by
  \code{plot.spec}: this is asymmetric, and the width of the centre
  mark indicates the equivalent bandwidth.
}
\note{
  The default plot for \code{spec} objects is quite complex, including an
  error bar and default title, subtitle and axis labels. The defaults can
  all be overridden by supplying the appropriate graphical parameters.
}
\references{
  Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
    Introduction.} Wiley.

  Brockwell, P. J. and Davis, R. A. (1991) \emph{Time Series: Theory and
    Methods.} Second edition. Springer.

  Venables, W. N. and Ripley, B. D. (1997) \emph{Modern Applied
    Statistics with S-PLUS.} Second edition. Springer. (Especially
  pp. 437--442.)
}
\author{Martyn Plummer, B.D. Ripley}
\seealso{\code{\link{spec.pgram}}}

\examples{
## Examples from Venables & Ripley
## spec.pgram
par(mfrow=c(2,2))
data(lh)
spectrum(lh)
spectrum(lh, spans=3)
spectrum(lh, spans=c(3,3))
spectrum(lh, spans=c(3,5))

data(UKLungDeaths)
spectrum(ldeaths)
spectrum(ldeaths, spans=c(3,3))
spectrum(ldeaths, spans=c(3,5))
spectrum(ldeaths, spans=c(5,7))
spectrum(ldeaths, spans=c(5,7), log="dB", ci=0.8)

# for multivariate examples see the help for spec.pgram

## spec.ar
spectrum(lh, method="ar")
spectrum(ldeaths, method="ar")
}
\keyword{ts}
