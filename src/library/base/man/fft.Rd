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

  The FFT is fastest when the length of of the series being transformed
  is highly composite (i.e. has many factors).  If this is not the case,
  the transform may take a long time to compute and will use a large
  amount of memory.
}
\references{
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
all(fft(fft(x), inverse = TRUE)/(x*length(x)) == 1+0i)
eps <- 1e-11 ## In general, not exactly, but still:
for(N in 1:130) {
    cat("N=",formatC(N,wid=3),": ")
    x <- rnorm(N)
    if(N \%\% 5 == 0) {
        m5 <- matrix(x,ncol=5)
        cat("mvfft:",all(apply(m5,2,fft) == mvfft(m5)),"")
    }
    dd <- Mod(1 - (f2 <- fft(fft(x), inverse=TRUE)/(x*length(x))))
    cat(if(all(dd < eps))paste(" all < ", formatC(eps)) else
            paste("NO: range=",paste(formatC(range(dd)),collapse=",")),"\n")
}

plot(fft(c(9:0,0:13, numeric(301))), type = "l")
periodogram <- function(x, mean.x = mean(x)) { # simple periodogram
  n <- length(x)
  x <- unclass(x) - mean.x
  Mod(fft(x))[2:(n\%/\%2 + 1)]^2 / (2*pi*n) # drop I(0)
}
data(sunspots)
plot(10*log10(periodogram(sunspots)), type = "b", col = "blue")
}
\keyword{math}
\keyword{dplot}
