\name{density}
\title{Kernel Density Estimation}
\usage{
density(x, bw, adjust = 1, kernel="gaussian", window = kernel,
        n = 512, width, from, to, cut = 3, na.rm = FALSE)
print(dobj)
plot(dobj, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE, \dots)
}
\alias{density}
\alias{print.density}
\alias{plot.density}
\arguments{
\item{x}{the data from which the estimate is to be computed.}
\item{n}{the number of equally spaced points at which the density
  is to be estimated.  When \code{n > 512}, it is rounded up to the next
  power of 2 for efficieny reasons (\code{\link{fft}}).}
\item{kernel,window}{a character string giving the smoothing kernel to be used.
  This must be one of \code{"gaussian"}, \code{"rectangular"},
  \code{"triangular"}, or \code{"cosine"}, and may be abbreviated to a
  single letter.}
\item{bw}{the smoothing bandwith to be used.  This is the standard
  deviation of the smoothing kernel.  It defaults to 0.9 times the
  minimum of the standard deviation and the interquartile range divided by
  1.34 times the sample size to the negative one-fifth power
  (= Silverman's ``rule of thumb'').
  The specified value of \code{bw} is multiplied by \code{adjust}.}
\item{adjust}{the bandwith used is actually \code{adjust*bw}.
  This makes it easy to specify values like ``half the default'' bandwidth.}
\item{width}{this exists for compatibility with S.}
\item{from,to}{the left and right-most points of the grid at which the
  density is to be estimated.}
\item{cut}{by default, the values of \code{left} and \code{right} are
  \code{cut} bandwidths beyond the extremes of the data. This allows the
  estimated density to drop to approximately zero at the extremes.}
\item{na.rm}{logical; if \code{TRUE}, missing values are removed
  from \code{x}. If \code{FALSE} any missing values cause an error.}
\item{dobj}{a ``density'' object.}
\item{main, xlab, ylab, type}{plotting parameters with useful defaults.}
\item{\dots}{further plotting parameters.}
\item{zero.line}{logical; if \code{TRUE}, add a base line at \eqn{y = 0}}
}
\description{
  The function \code{density} computes kernel density estimates
  with the given kernel and bandwidth.

  The generic functions \code{plot} and \code{print} have
  methods for density objects.
}
\details{
The algorithm used in \code{density} disperses the mass of the
empirical distribution function over a regular grid of at least 512
points and then
uses the fast Fourier transform to convolve this approximation
with a discretized version of the kernel and then uses linear
approximation to evaluate the density at the specified points.

Infinite values in \code{x} are assumed to correspond to a point mass at
\code{+/-Inf} and the density estimate is of the sub-density on
\code{(-Inf, +Inf)}.
}
\value{
An object with class \code{"density"}.
The underlying structure is a list containing the following components.
\item{x}{the \code{n} coordinates of the points where the density is estimated.}
\item{y}{the estimated density values.}
\item{bw}{the bandwidth used.}
\item{N}{the sample size after elimination of missing values.}
\item{call}{the call which produced the result.}
\item{data.name}{the deparsed name of the \code{x} argument.}
\item{has.na}{logical, for compatibility (always FALSE).}
}
\references{
Silverman, B. W. (1986).
\emph{Density Estimation}.
London: Chapman and Hall.

Venables, W. N. and B. D. Ripley (1994,7,9).
\emph{Modern Applied Statistics with S-PLUS}.
New York: Springer.

Scott, D. W. (1992).
\emph{Multivariate Density Estimation.  Theory, Practice and Visualization}.
New York: Wiley.

Sheather, S. J. and M. C. Jones (1991).
``A reliable data-based bandwidth selection method for kernel density
estimation.
\emph{J. Roy. Statist. Soc.} \bold{B}, 683-690.
}
\seealso{
\code{\link{convolve}}, \code{\link{hist}}.
}
\examples{
# The Old Faithful geyser data
data(faithful)
d <- density(faithful$eruptions, bw=0.15)
d
plot(d)

plot(d, type="n")
polygon(d, col="wheat")

## Missing values:
x <- xx <- faithful$eruptions
x[i.out <- sample(length(x), 10)] <- NA
doR <- density(x, bw=0.15, na.rm = TRUE)
lines(doR, col="blue")
points(xx[i.out], rep(.01,10))
}
\keyword{dplot}
\keyword{distribution}
\keyword{smooth}
