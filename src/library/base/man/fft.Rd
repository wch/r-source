\name{fft}
\title{Discrete Fourier Transform}
\usage{
fft(z, inverse = FALSE)
mvfft(z, inverse = FALSE)
}
\alias{fft}
\alias{mvfft}
\arguments{
  \item{z}{a real or complex value array containing the values to be
    transformed}
  \item{inverse}{if \code{TRUE}, the inverse transform is computed (the
    inverse has a \code{+} in the exponent of \emph{e} and divides the
    values by \code{1/length(x)} before transforming).}
}
\value{
  When \code{z} is a vector, the value computed and returned by
  \code{fft} is the univariate Fourier transform of the sequence of
  values in \code{z}.  When \code{z} contains an array, \code{fft}
  computes and returns the multivariate (spatial) transform.

  By contrast, \code{mvfft} takes a real or complex matrix as argument,
  and returns a similar shaped matrix, but with each column replaced by
  its discrete Fourier transform.  This is useful for analyzing
  vector-valued series.

  The FFT is fastest when the length of of the series being transformed
  is highly composite (i.e. has many factors).  If this is not the case,
  the transform may take a long time to compute and will use a large
  amount of memory.
}
\references{
  Singleton, R. C. (1979).
  Mixed Radix Fast Fourier Transforms,
  in \emph{Programs for Digital Signal Processing},
  IEEE Digital Signal Processing Committee eds.
  IEEE Press.
}
\seealso{
  \code{\link{convolve}}, \code{\link{nextn}}.
}
\examples{
plot(fft(c(9:0,0:13, numeric(301))), type = "l")
periodogram <- function(x) { # simple periodogram
  n <- length(x)
  Mod(fft(unclass(x)))[1:(n\%/\%2 + 1)]^2 / (2*pi*n)
}
data(sunspots)
plot(10*log10(periodogram(sunspots)), type = "b", col = "blue")
}
\keyword{math}
\keyword{dplot}
