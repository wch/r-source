\name{density}
\title{Kernel Density Estimation}
\usage{
density(x, bw, adjust = 1,
        kernel=c("gaussian", "epanechnikov", "rectangular", "triangular",
                 "biweight", "cosine", "optcosine"),
        window = kernel, width,
        give.Rkern = FALSE,
        n = 512, from, to, cut = 3, na.rm = FALSE)
print(dobj)
plot(dobj, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE, \dots)
}
\alias{density}
\alias{print.density}
\alias{plot.density}
\arguments{
\item{x}{the data from which the estimate is to be computed.}
\item{bw}{the smoothing bandwidth to be used.  The kernels are scaled
  such that this is the standard deviation of the smoothing kernel.
  It defaults to 0.9 times the
  minimum of the standard deviation and the interquartile range divided by
  1.34 times the sample size to the negative one-fifth power
  (= Silverman's ``rule of thumb'') \emph{unless} the quartiles
  coincide where \code{bw > 0} will be guaranteed.
  The specified (or default) value of \code{bw} is multiplied by
  \code{adjust}.
}
\item{adjust}{the bandwidth used is actually \code{adjust*bw}.
  This makes it easy to specify values like ``half the default'' bandwidth.}
\item{kernel,window}{a character string giving the smoothing kernel to be used.
  This must be one of \code{"gaussian"}, \code{"rectangular"},
  \code{"triangular"}, \code{"epanechnikov"}, \code{"biweight"},
  \code{"cosine"} or \code{"optcosine"},
  with default \code{"gaussian"},
  and may be abbreviated to a unique prefix (single letter).

  \code{"cosine"} is smoother than \code{"optcosine"}, which is the
  usual ``cosine'' kernel in the literature and almost MSE-efficient.
}
\item{width}{this exists for compatibility with S; if given, and
  \code{bw} is not, will set \code{bw = width/4}.}
\item{give.Rkern}{logical; if true, \emph{no} density is estimated, and
  the ``canonical bandwidth'' of the chosen \code{kernel} is returned instead.}
\item{n}{the number of equally spaced points at which the density
  is to be estimated.  When \code{n > 512}, it is rounded up to the next
  power of 2 for efficieny reasons (\code{\link{fft}}).}
\item{from,to}{the left and right-most points of the grid at which the
  density is to be estimated.}
\item{cut}{by default, the values of \code{left} and \code{right} are
  \code{cut} bandwidths beyond the extremes of the data. This allows the
  estimated density to drop to approximately zero at the extremes.}
\item{na.rm}{logical; if \code{TRUE}, missing values are removed
  from \code{x}. If \code{FALSE} any missing values cause an error.}
%%---- Those for the methods:
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
  points and then uses the fast Fourier transform to convolve this
  approximation with a discretized version of the kernel and then uses
  linear approximation to evaluate the density at the specified points.

  The statistical properties of a kernel are determined by
  \eqn{\sigma^2_K = \int t^2 K(t) dt}{sig^2 (K) = int(t^2 K(t) dt)}
  which is always \eqn{= 1} for our kernels (and hence the bandwidth
  \code{bw} is the standard deviation of the kernel) and
  \eqn{R(K) = \int K^2(t) dt}{R(K) = int(K^2(t) dt)}.\cr
  MSE-equivalent bandwidths (for different kernels) are proportional to
  \eqn{\sigma_K R(K)}{sig(K) R(K)} which is scale invariant and for our
  kernels equal to \eqn{R(K)}.  This value is returned when
  \code{give.Rkern = TRUE}.  See the examples for using exact equivalent
  bandwidths.

  Infinite values in \code{x} are assumed to correspond to a point mass at
  \code{+/-Inf} and the density estimate is of the sub-density on
  \code{(-Inf, +Inf)}.
}
\value{
  If \code{give.Rkern} is true, the number \eqn{R(K)}, otherwise
  an object with class \code{"density"} whose
  underlying structure is a list containing the following components.
  \item{x}{the \code{n} coordinates of the points where the density is
    estimated.}
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
plot(density(c(-20,rep(0,98),20)),xlim=c(-4,4))# IQR = 0

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


(kernels <- eval(formals(density)$kernel))

plot (density(0,bw=1))
for(i in 2:length(kernels))
   lines(density(0,bw=1, kern= kernels[i]), col=i)
mtext(side=3, "R's density() kernels with bw=1")
legend(1.5,.4, leg=kernels, col=seq(kernels),lty=1, cex=.8, y.int=1)

(RKs <- cbind(sapply(kernels, function(k)density(kern=k, give.Rkern=TRUE))))
100*round(RKs["epanechnikov",]/RKs, 4) ## Efficiencies

data(precip)
plot(density(precip, n=2^13))
for(i in 2:length(kernels))
   lines(density(precip, kern= kernels[i], n=2^13), col=i)
mtext(side=3, "same scale bandwidths, 7 different kernels")

## Bandwidth Adjustment for "Exactly Equivalent Kernels"
h.f <- sapply(kernels, function(k)density(kern=k, give.Rkern=TRUE))
(h.f <- (h.f["gaussian"] / h.f)^ .2)
## -> 1, 1.01, .995, 1.007,... close to 1 => adjustment barely visible..

plot(density(precip, n=2^13))
for(i in 2:length(kernels))
   lines(density(precip, adjust = h.f[i], kern= kernels[i], n=2^13), col=i)
mtext(side=3, "equivalent bandwidths, 7 different kernels")
legend(55,.035, leg=kernels, col=seq(kernels),lty=1)
}
\keyword{dplot}
\keyword{distribution}
\keyword{smooth}
