\name{spec.pgram}
\alias{spec.pgram}
\title{Estimate Spectral Density of a Time Series from Smoothed Periodogram}
\usage{
spec.pgram(x, spans=NULL, kernel, taper=0.1, demean=FALSE, detrend=TRUE,
	pad=0, fast=TRUE, plot=FALSE, na.action, ...)
}
\arguments{
 \item{x}{A univariate or multivariate time series.}
 \item{spans}{Vector of odd integers giving the widths of modified Daniell
  smoothers to be used to smooth the periodogram.}
 \item{kernel}{Alternatively, a kernel smoother of class \code{"kernel"}.}
 \item{taper}{Proportion of data to taper.  A split cosine bell taper is
  applied to this proportion of the data at the beginning and end of
  the series.}
 \item{demean}{logical. If \code{TRUE}, subtract the mean of the series.}
 \item{detrend}{logical. If \code{TRUE}, remove a linear trend from the
    series. This will also remove the mean.}
 \item{pad}{Proportion of data to pad. Zeros are added to the end of
  the series to increase its length by the proportion \code{pad}.}
 \item{plot}{Plot the periodogram?}
 \item{fast}{If true, pad the series to a highly composite length.}
 \item{\dots}{Graphical arguments passed to \code{plot.spec}.}
}\description{
    \code{spec.pgram} calculates the periodogram using a fast Fourier
    transform, and optionally smooths the result with a series of modified
    Daniell smoothers (moving averages giving half weight to the end values).
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
}
\value{
   An object of class \code{\link{spec}} with the following additional
   list elements.
   \item{kernel}{The \code{kernel} argument, or the kernel constructed
       from \code{spans}.}
   \item{df}{The distribution of the spectral density estimate can
      be approximated by a chi square distribution with \code{df}
      degrees of freedom.}
   \item{bandwidth}{The equivalent bandwidth of the kernel smoother as
       defined by Bloomfield (1976, p.201).}
   \item{taper}{The value of the \code{taper} argument.}
   \item{pad}{The value of the \code{pad} argument.}
   \item{detrend}{The value of the \code{detrend} argument.}
   \item{demean}{The value of the \code{demean} argument.}
}

\references{
    Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
	Introduction.} Wiley.

    Brockwell, P.J. and Davis, R.A. (1991) \emph{Time Series: Theory and
	Methods.} Second edition. Springer.

    Venables, W.N. and Ripley, B.D. (1997) \emph{Modern Applied
	Statistics with S-PLUS.} Second edition. Springer. (Especially
        pp. 437-442.)
}
\author{Originally Martyn Plummer; kernel smoothing by Adrian Trapetti,
    synthesis by B.D. Ripley}
\seealso{\code{\link{spectrum}}, \code{\link{spec.taper}},
    \code{\link{plot.spec}}, \code{\link{fft}}}
\examples{
data(deaths)
spectrum(deaths)
spectrum(deaths, spans=c(3,5))
spectrum(deaths, spans=c(5,7))
spectrum(mdeaths, spans=c(3,3))
spectrum(fdeaths, spans=c(3,3))

## bivariate example
mfdeaths.spc <- spec.pgram(ts.union(mdeaths, fdeaths), 
   spans=c(3,3))
plot(mfdeaths.spc)  # plot both marginal spectra

# plot coherency
plot(mfdeaths.spc$freq, mfdeaths.spc$coh, type="l", 
   ylim=c(0,1), xlab="coherency", ylab="")
gg <- 2/mfdeaths.spc$df
se <- sqrt(gg/2)
lines(mfdeaths.spc$freq, tanh(atanh(mfdeaths.spc$coh) + 
   1.96*se), lty=3)
lines(mfdeaths.spc$freq, tanh(atanh(mfdeaths.spc$coh) - 
   1.96*se), lty=3)
#plot phase
plot(mfdeaths.spc$freq, mfdeaths.spc$phase, type="l", 
   ylim=c(-pi, pi), xlab="phase spectrum", ylab="")
cl <- asin( pmin( 0.9999, qt(0.95, 2/gg-2)*
   sqrt(gg*(mfdeaths.spc$coh^{-2} - 1)/(2*(1-gg)) ) ) )
lines(mfdeaths.spc$freq, mfdeaths.spc$phase + cl, lty=3)
lines(mfdeaths.spc$freq, mfdeaths.spc$phase - cl, lty=3)

## now impose a lack of alignment
mfdeaths.spc <- spec.pgram(ts.intersect(mdeaths, lag(fdeaths, 4)),
   spans=c(3,3))
plot(mfdeaths.spc$freq, mfdeaths.spc$coh, type="l", 
   ylim=c(0,1), xlab="coherency", ylab="")
gg <- 2/mfdeaths.spc$df
se <- sqrt(gg/2)
lines(mfdeaths.spc$freq, tanh(atanh(mfdeaths.spc$coh) + 
   1.96*se), lty=3)
lines(mfdeaths.spc$freq, tanh(atanh(mfdeaths.spc$coh) - 
   1.96*se), lty=3)
phase <- (mfdeaths.spc$phase + pi)%%(2*pi) - pi
plot(mfdeaths.spc$freq, phase, type="l", 
   ylim=c(-pi, pi), xlab="phase spectrum", ylab="")
cl <- asin( pmin( 0.9999, qt(0.95, 2/gg-2)*
   sqrt(gg*(mfdeaths.spc$coh^{-2} - 1)/(2*(1-gg)) ) ) )
lines(mfdeaths.spc$freq, phase + cl, lty=3)
lines(mfdeaths.spc$freq, phase - cl, lty=3)
}
\keyword{ts}
