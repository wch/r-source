\name{density}
\title{Kernel Density Estimation}
\usage{
density(x, n=512, kernel="gaussian", bw, adjust=1, width,
        from, to, cut=3)

bw.ucv(x, samples=100)
bw.bcv(x, samples=100)
bw.sj(x, samples=100)

print(dobj)
plot(dobj, \dots)
}
\alias{density}
\alias{bw.ucv}
\alias{bw.bcv}
\alias{bw.sj}
\alias{print.density}
\alias{plot.density}
\arguments{
\item{x}{the values for which the estimate is to be computed.}
\item{n}{the number of equally spaced points at which the density
is to be estimated.  This is rounded up to the next power of 2,
with a minimum value of 512.}
\item{kernel}{a character string giving the smoothing kernel to be used.
This must be one of \code{"gaussian"}, \code{"rectangular"}, \code{"triangular"},
or \code{"cosine"}, and may be abbrevited to a single letter.}
\item{bw}{the smoothing bandwith to be used.  This is the standard
deviation of the smoothing kernel.  It defaults to 1.06 times the
minimum of the standard deviation and the interquartile range divided by
1.34 times the sample size to the negative one fifth power.
The specified value of \code{bw} is multiplied by \code{adjust}.}
\item{adjust}{the bandwith used is actually \code{adjust*bw}.
This makes it easy to specify values like ``half the default'' bandwidth.}
\item{width}{this exists for compatibility with S.}
\item{from,to}{the left and right-most points of the grid at which the
density is to be estimated.}
\item{cut}{by default, the values of \code{left} and \code{right} are
\code{cut} bandwiths beyond the extremes of the data.}
\item{samples}{the sample size to take in the bandwidth
selection functions. If \code{samples} is non-positive, the entire
data set is used.}
\item{dobj}{a ``density'' object.}
\item{\dots}{plotting parameters.}
}
\description{
The function \code{density} computes kernel density estimates
with the given kernel and bandwidth
(which is the standard deviation of the kernel).

The functions \code{bw.ucv}, \code{bw.bcv} and \code{bw.sj}
provide automated ways of selecting a bandwith for density
estimates (assuming a Gaussian kernel).
The techniques used are as follows; \code{bw.ucv} uses unbiased
cross-validation, \code{bw.bcv} uses biased cross-validation and
\code{bw.sj} uses the technique of Sheather and Jones.
These cross-validation techniques are based on code from the
Venables and Ripley MASS package.

The generic functions \code{plot} and \code{print} have
methods for density objects.

The algorithm used in \code{density} disperses the mass of the
empirical distribution function over a regular grid and then
uses the fast Fourier transform to convolve this approximation
with a discretized version of the kernel.
}
\value{
An object with class ``density''.
The underlying structure is a list containing the following components.
\item{x}{the coordinates of the points where the density is estimated.}
\item{y}{the estimated density values.}
\item{bw}{the bandwidth used.}
\item{n}{the sample size \code{length(x)}.}
\item{call}{the call which produced the result.}
\item{data.name}{the deparsed name of the \code{x} argument.}
}
\references{
Silverman, B. W. (1986).
\emph{Density Estimation}.
London: Chapman and Hall.

Venables, W. N. and B. D. Ripley (1994).
\emph{Modern Applied Statistics with S-Plus}.
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
}
\keyword{dplot}
\keyword{distribution}
\keyword{smooth}
