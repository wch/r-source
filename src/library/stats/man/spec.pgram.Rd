% File src/library/stats/man/spec.pgram.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{spec.pgram}
\alias{spec.pgram}
\title{Estimate Spectral Density of a Time Series by a Smoothed
    Periodogram}
\usage{
spec.pgram(x, spans = NULL, kernel, taper = 0.1,
           pad = 0, fast = TRUE, demean = FALSE, detrend = TRUE,
           plot = TRUE, na.action = na.fail, \dots)
}
\arguments{
  \item{x}{univariate or multivariate time series.}
  \item{spans}{vector of odd integers giving the widths of modified
    Daniell smoothers to be used to smooth the periodogram.}
  \item{kernel}{alternatively, a kernel smoother of class
    \code{"tskernel"}.}
  \item{taper}{specifies the proportion of data to taper.  A split
    cosine bell taper is applied to this proportion of the data at the
    beginning and end of the series.}
  \item{pad}{proportion of data to pad. Zeros are added to the end of
    the series to increase its length by the proportion \code{pad}.}
  \item{fast}{logical; if \code{TRUE}, pad the series to a highly composite
    length.}
  \item{demean}{logical. If \code{TRUE}, subtract the mean of the
    series.}
  \item{detrend}{logical. If \code{TRUE}, remove a linear trend from
    the series. This will also remove the mean.}
  \item{plot}{plot the periodogram?}
  \item{na.action}{\code{NA} action function.}
  \item{\dots}{graphical arguments passed to \code{plot.spec}.}
}
\description{
  \code{spec.pgram} calculates the periodogram using a fast Fourier
  transform, and optionally smooths the result with a series of
  modified Daniell smoothers (moving averages giving half weight to
  the end values).
}
\details{
  The raw periodogram is not a consistent estimator of the spectral density,
  but adjacent values are asymptotically independent. Hence a consistent
  estimator can be derived by smoothing the raw periodogram, assuming that
  the spectral density is smooth.

  The series will be automatically padded with zeros until the series
  length is a highly composite number in order to help the Fast Fourier
  Transform. This is controlled by the \code{fast} and not the \code{pad}
  argument.

  The periodogram at zero is in theory zero as the mean of the series
  is removed (but this may be affected by tapering): it is replaced by
  an interpolation of adjacent values during smoothing, and no value
  is returned for that frequency.
}
\value{
  A list object of class \code{"spec"} (see \code{\link{spectrum}})
  with the following additional components:
  \item{kernel}{The \code{kernel} argument, or the kernel constructed
    from \code{spans}.}
  \item{df}{The distribution of the spectral density estimate can be
    approximated by a (scaled) chi square distribution with \code{df} degrees
    of freedom.}
  \item{bandwidth}{The equivalent bandwidth of the kernel smoother as
    defined by Bloomfield (1976, page 201).}
  \item{taper}{The value of the \code{taper} argument.}
  \item{pad}{The value of the \code{pad} argument.}
  \item{detrend}{The value of the \code{detrend} argument.}
  \item{demean}{The value of the \code{demean} argument.}

  The result is returned invisibly if \code{plot} is true.
}

\references{
  Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
    Introduction.} Wiley.

  Brockwell, P.J. and Davis, R.A. (1991) \emph{Time Series: Theory and
    Methods.} Second edition. Springer.

  Venables, W.N. and Ripley, B.D. (2002) \emph{Modern Applied
    Statistics with S.} Fourth edition. Springer.
  (Especially pp.\sspace{}392--7.)
}
\author{
  Originally Martyn Plummer; kernel smoothing by Adrian Trapletti,
  synthesis by B.D. Ripley
}
\seealso{\code{\link{spectrum}}, \code{\link{spec.taper}},
  \code{\link{plot.spec}}, \code{\link{fft}}}

\examples{
require(graphics)

## Examples from Venables & Ripley
spectrum(ldeaths)
spectrum(ldeaths, spans = c(3,5))
spectrum(ldeaths, spans = c(5,7))
spectrum(mdeaths, spans = c(3,3))
spectrum(fdeaths, spans = c(3,3))

## bivariate example
mfdeaths.spc <- spec.pgram(ts.union(mdeaths, fdeaths), spans = c(3,3))
# plots marginal spectra: now plot coherency and phase
plot(mfdeaths.spc, plot.type = "coherency")
plot(mfdeaths.spc, plot.type = "phase")

## now impose a lack of alignment
mfdeaths.spc <- spec.pgram(ts.intersect(mdeaths, lag(fdeaths, 4)),
   spans = c(3,3), plot = FALSE)
plot(mfdeaths.spc, plot.type = "coherency")
plot(mfdeaths.spc, plot.type = "phase")

stocks.spc <- spectrum(EuStockMarkets, kernel("daniell", c(30,50)),
                       plot = FALSE)
plot(stocks.spc, plot.type = "marginal") # the default type
plot(stocks.spc, plot.type = "coherency")
plot(stocks.spc, plot.type = "phase")

sales.spc <- spectrum(ts.union(BJsales, BJsales.lead),
                      kernel("modified.daniell", c(5,7)))
plot(sales.spc, plot.type = "coherency")
plot(sales.spc, plot.type = "phase")
}
\keyword{ts}
