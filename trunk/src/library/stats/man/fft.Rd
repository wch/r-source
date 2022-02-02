% File src/library/stats/man/fft.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{fft}
\alias{fft}
\alias{mvfft}
\title{Fast Discrete Fourier Transform (FFT)}
\description{
  Computes the Discrete Fourier Transform (DFT) of an array with a fast
  algorithm, the \dQuote{Fast Fourier Transform} (FFT).
}
\usage{
fft(z, inverse = FALSE)
mvfft(z, inverse = FALSE)
}
\arguments{
  \item{z}{a real or complex array containing the values to be
    transformed.  Long vectors are not supported.}
  \item{inverse}{if \code{TRUE}, the unnormalized inverse transform is
    computed (the inverse has a \code{+} in the exponent of \eqn{e},
    but here, we do \emph{not} divide by \code{1/length(x)}).}
}
\value{
  When \code{z} is a vector, the value computed and returned by
  \code{fft} is the unnormalized univariate discrete Fourier transform of the
  sequence of values in \code{z}.  Specifically, \code{y <- fft(z)} returns
  \deqn{y[h] = \sum_{k=1}^n z[k] \exp(-2\pi i (k-1) (h-1)/n)}{%
        y[h] =  sum_{k=1}^n z[k]*exp(-2*pi*1i*(k-1)*(h-1)/n)}
  for \eqn{h = 1,\ldots,n}{h = 1, ..., n} where n = \code{length(y)}.  If
  \code{inverse} is \code{TRUE}, \eqn{\exp(-2\pi\ldots)}{exp(-2*pi...)}
  is replaced with \eqn{\exp(2\pi\ldots)}{exp(2*pi...)}.

  When \code{z} contains an array, \code{fft} computes and returns the
  multivariate (spatial) transform.  If \code{inverse} is \code{TRUE},
  the (unnormalized) inverse Fourier transform is returned, i.e.,
  if \code{y <- fft(z)}, then \code{z} is
  \code{fft(y, inverse = TRUE) / length(y)}.

  By contrast, \code{mvfft} takes a real or complex matrix as argument,
  and returns a similar shaped matrix, but with each column replaced by
  its discrete Fourier transform.  This is useful for analyzing
  vector-valued series.

  The FFT is fastest when the length of the series being transformed
  is highly composite (i.e., has many factors).  If this is not the
  case, the transform may take a long time to compute and will use a
  large amount of memory.
}
\source{
  Uses C translation of Fortran code in Singleton (1979).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Singleton, R. C. (1979).
  Mixed Radix Fast Fourier Transforms,
  in \emph{Programs for Digital Signal Processing},
  IEEE Digital Signal Processing Committee eds.
  IEEE Press.

  Cooley, James W., and Tukey, John W. (1965).
  An algorithm for the machine calculation of complex Fourier series,
  \emph{Mathematics of Computation}, \bold{19}(90), 297--301.
  \doi{10.2307/2003354}.
}
\seealso{
  \code{\link{convolve}}, \code{\link{nextn}}.
}
\examples{
x <- 1:4
fft(x)
fft(fft(x), inverse = TRUE)/length(x)

## Slow Discrete Fourier Transform (DFT) - e.g., for checking the formula
fft0 <- function(z, inverse=FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

relD <- function(x,y) 2* abs(x - y) / abs(x + y)
n <- 2^8
z <- complex(n, rnorm(n), rnorm(n))
\donttest{## relative differences in the order of 4*10^{-14} :
summary(relD(fft(z), fft0(z)))
summary(relD(fft(z, inverse=TRUE), fft0(z, inverse=TRUE)))
}}
\keyword{math}
\keyword{dplot}
