\name{kernel}
\alias{kernel}
\alias{bandwidth.kernel}
\alias{df.kernel}
\alias{is.tskernel}
\alias{print.tskernel}
\alias{plot.tskernel}
\alias{[.tskernel}

\title{Smoothing Kernel Objects}
\usage{
kernel(coef, m, r, name)

df.kernel(k)
bandwidth.kernel(k)
is.tskernel(k)

print(k, digits = max(3,getOption("digits")-3))
plot(k)
}
\arguments{
  \item{coef}{the upper half of the smoothing kernel coefficients
    (inclusive of coefficient zero) \emph{or} the name of a kernel
    (currently \code{"daniell"}, \code{"dirichlet"}, \code{"fejer"} or
    \code{"modified.daniell"}.}
  \item{m}{the kernel dimension. The number of kernel coefficients is
    \code{2*m+1}.}
  \item{name}{the name of the kernel.}
  \item{r}{the kernel order for a Fejer kernel.}
  \item{digits}{the number of digits to format real numbers.}
}
\description{
  The \code{"tskernel"} class is designed to represent discrete symmetric
  normalized smoothing kernels. These kernels can be used to smooth
  vectors, matrices, or time series objects.
}
\details{
  \code{kernel} is used to construct a general kernel or
  named specific kernels. The modified Daniell kernel
  halves the end coefficients (as used by S-PLUS).
  
  \code{df.kernel} returns the "equivalent degrees of freedom" of a
  smoothing kernel as defined in Brockwell and Davies (1991), p. 362,
  and \code{bandwidth.kernel} returns the equivalent bandwidth as
  defined in Bloomfield (1991), p. 201, with a continuity correction.
}
\value{
  \code{kernel} returns a list with class \code{"tskernel"}, and
  components the coefficients \code{coef} and the kernel dimension
  \code{m}. An additional attribute is \code{"name"}.
}
\author{A. Trapletti; modifications by B.D. Ripley}
\seealso{
    \code{\link{kernapply}}
}
\references{
  Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
    Introduction.} Wiley.

  Brockwell, P.J. and Davis, R.A. (1991) \emph{Time Series: Theory and
    Methods.} Second edition. Springer, pp. 350--365.
}
\examples{
data(EuStockMarkets)    # Demonstrate a simple trading strategy for the 
x <- EuStockMarkets[,1]  # financial time series German stock index DAX.
k1 <- kernel("daniell", 50)  # a long moving average
k2 <- kernel("daniell", 10)  # and a short one
plot(k1) 
plot(k2)
x1 <- kernapply(x, k1)
x2 <- kernapply(x, k2)
plot(x)
lines(x1, col = "red")    # go long if the short crosses the long upwards
lines(x2, col = "green")  # and go short otherwise

data(sunspot)     # Reproduce example 10.4.3 from Brockwell and Davies (1991)
spectrum(sunspot.year, kernel=kernel("daniell", c(11,7,3)), log="no")
}
\keyword{ts}

