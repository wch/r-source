\name{kernapply}
\title{Apply Smoothing Kernel}
\alias{kernapply}
\alias{kernapply.default}
\alias{kernapply.ts}
\alias{kernapply.tskernel}
\alias{kernapply.vector}
\usage{
kernapply(x, \dots)

\method{kernapply}{default}(x, k, circular = FALSE, \dots)
\method{kernapply}{ts}(x, k, circular = FALSE, \dots)
\method{kernapply}{vector}(x, k, circular = FALSE, \dots)

\method{kernapply}{tskernel}(x, k, \dots)
}
}
\arguments{
  \item{x}{an input vector, matrix, time series or kernel to be smoothed.}
  \item{k}{smoothing \code{"tskernel"} object.}
  \item{circular}{a logical indicating whether the input sequence to be
    smoothed is treated as circular, i.e., periodic.}
  \item{\dots}{arguments passed to or from other methods.}
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
## see 'kernel' for examples
}
\keyword{ts}
