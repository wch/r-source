\name{kernapply}
\title{Apply Smoothing Kernel}
\alias{kernapply}
\alias{kernapply.default}
\alias{kernapply.ts}
\alias{kernapply.tskernel}
\alias{kernapply.vector}
\synopsis{
kernapply(x, ...)
kernapply.default(x, k, circular = FALSE)
kernapply.ts(x, k, circular = FALSE)
kernapply.vector(x, k, circular = FALSE)
kernapply.tskernel(k1, k2)
}
\usage{
kernapply(x, k, circular = FALSE) 
kernapply(k1, k2)
}
\arguments{
  \item{k, k1, k2}{smoothing \code{"tskernel"} objects.}
  \item{x}{an input vector, matrix, or time series to be smoothed.}
  \item{circular}{a logical indicating whether the input sequence to be
    smoothed is treated as circular, i.e., periodic.}
}
\description{
  \code{kernapply} computes the convolution between an input sequence 
  and a specific kernel.
}
\value{
  A smoothed version of the input sequence.
}
\author{A. Trapletti}
\seealso{
  \code{\link{kernel}}, \code{\link{convolve}}, \code{\link{filter}},
  \code{\link{spectrum}}
}
\examples{
## see `kernel' for examples
}
\keyword{ts}
