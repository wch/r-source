% File src/library/stats/man/fft.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{fft}
\alias{fft}
\alias{mvfft}
\title{Fast Discrete Fourier Transform}
\description{
  Performs the Fast Fourier Transform of an array.
}
\usage{
fft(z, inverse = FALSE)
mvfft(z, inverse = FALSE)
}
\arguments{
  \item{z}{a real or complex array containing the values to be
    transformed.}
  \item{inverse}{if \code{TRUE}, the unnormalized inverse transform is
    computed (the inverse has a \code{+} in the exponent of \eqn{e},
    but here, we do \emph{not} divide by \code{1/length(x)}).}
}
\value{
  When \code{z} is a vector, the value computed and returned by
  \code{fft} is the unnormalized univariate Fourier transform of the
  sequence of values in \code{z}.
  %%
  %% Here, we should really have a nice  \deqn{}{} giving the definition
  %% of the DFT !
  %%
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
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Singleton, R. C. (1979)
  Mixed Radix Fast Fourier Transforms,
  in \emph{Programs for Digital Signal Processing},
  IEEE Digital Signal Processing Committee eds.
  IEEE Press.
}
\seealso{
  \code{\link{convolve}}, \code{\link{nextn}}.
}
\examples{
x <- 1:4
fft(x)
fft(fft(x), inverse = TRUE)/length(x)
}
\keyword{math}
\keyword{dplot}
