\name{kernel}
\alias{kernel}
\alias{daniell.kernel}
\alias{modified.daniell.kernel}
\alias{fejer.kernel}
\alias{dirichlet.kernel}
\alias{df.kernel}
\alias{is.kernel}
\alias{print.kernel}
\alias{plot.kernel}
\alias{"[.kernel"}
\alias{apply.kernel}
\alias{apply.kernel.default}
\alias{apply.kernel.ts}
\alias{apply.kernel.kernel}

\title{Smoothing Kernel Objects}
\usage{
kernel (coef, m, name)
daniell.kernel (m)
modified.daniell.kernel (m)
fejer.kernel (m, r)
dirichlet.kernel (m, r)

df.kernel (k)
bandwidth.kernel (k)
is.kernel (k)

print (k, digits = max(3,.Options$digits-3))
plot (k)
k[i]
apply.kernel (x, k, circular = FALSE) 
apply.kernel (k1, k2)
}
\arguments{
  \item{coef}{the upper half of the smoothing kernel coefficients
    (inclusive coefficient zero).}
  \item{m}{the kernel dimension. The number of kernel coefficients is
    \code{2*m+1}.}
  \item{name}{the name of the kernel.}
  \item{r}{the kernel order.}
  \item{k, k1, k2}{smoothing \code{"kernel"} objects.}
  \item{digits}{the number of digits to format real numbers.}
  \item{x}{an input vector, matrix, or time series to be smoothed.}
  \item{circular}{a logical indicating whether the input sequence to be
    smoothed is treated as circular, i.e., periodic.}
}
\description{
  The \code{"kernel"} class is designed to represent discrete symmetric
  normalized smoothing kernels. These kernels can be used to smooth
  vectors, matrices, or time series objects. Furthermore, they are used
  for the \code{\link{spectrum}} estimation and related routines.
  The \code{"kernel"} class also provides common \code{\link{plot}},
  \code{\link{print}}, and extract \code{\link{methods}}.
}
\details{
    \code{kernel}, \code{daniell.kernel},
    \code{modified.daniell.kernel}, \code{fejer.kernel}, and
    \code{dirichlet.kernel} are used to construct a general kernel and the
    named specific kernels, respectively. The modified Daniell kernel
    halves the end coefficients (as used by S-PLUS).
  
    \code{df.kernel} returns the "equivalent degrees of freedom" of a
    smoothing kernel as defined in Brockwell and Davies (1991), p. 362,
    and \code{bandwidth.kernel} returns the equivalent bandwidth as
    defined in Bloomfield (1991), p. 201, with a continuity correction.
  
    \code{apply.kernel} computes the convolution between an input sequence 
    and a specific kernel. See also \code{\link{convolve}} and
    \code{\link{filter}}.
}
\value{
    \code{kernel}, \code{daniell.kernel},
    \code{modified.daniell.kernel}, \code{fejer.kernel}, and
    \code{dirichlet.kernel} return \code{"kernel"} objects. These objects
    consist of a list with class \code{"kernel"}, the coefficients
    \code{coef}, and the kernel dimension \code{m}. An additional
    attribute is \code{"name"}.
  
    \code{k[i]} returns the \code{i}'th coefficient of the kernel.
  
    The \code{print} and \code{plot} methods return the invisible input
    object.

    \code{apply.kernel} returns the smoothed input sequence.
}
\author{A. Trapletti; modifications by B.D. Ripley}
\seealso{
    \code{\link{density}}, \code{\link{convolve}}, \code{\link{filter}},
    \code{\link{spectrum}}
}
\references{
    Bloomfield, P. (1976) \emph{Fourier Analysis of Time Series: An
	Introduction.} Wiley.

    Brockwell, P.J. and Davis, R.A. (1991) \emph{Time Series: Theory and
	Methods.} Second edition. Springer, pp. 350-365.
}
\examples{
data (finance)    # Demonstrate a simple trading strategy for the 
x <- finance[,1]  # financial time series German stock index DAX.
k1 <- daniell.kernel (50)  # a long moving average
k2 <- daniell.kernel (10)  # and a short one
plot (k1) 
plot (k2)
x1 <- apply.kernel (x, k1)
x2 <- apply.kernel (x, k2)
plot (x)
lines (x1, col = "red")    # go long if the short crosses the long upwards
lines (x2, col = "green")  # and go short otherwise

data (sunspot)     # Reproduce example 10.4.3 from Brockwell and Davies (1991)
x <- sunspot.year  # p. 354
k1 <- daniell.kernel (11)
k2 <- daniell.kernel (7)
k3 <- daniell.kernel (3)
k <- apply.kernel (k1, k2)
k <- apply.kernel (k, k3)
# or just k <- daniell.kernel(c(11,7,3))
spectrum (x, kernel=k)
}
\keyword{ts}

