\name{convolve}
\title{Fast Convolution}
\usage{
convolve(x, y, conj = TRUE, type = c("circular", "open", "filter"))
}
\alias{convolve}
\arguments{
\item{x,y}{numeric sequences \emph{of the same length} to be convolved.}
\item{conj}{logical; if \code{TRUE}, take the complex \emph{conjugate}
    before back-transforming (default, and used for usual convolution).}
\item{type}{character; one of \code{"circular"}, \code{"open"},
    \code{"filter"} (beginning of word is ok).
    For \code{circular}, the two sequences are treated as
    \emph{circular}, i.e., periodic.

    For \code{open} and \code{filter}, the sequences are padded with
    \code{0}s (from left and right) first;
    \code{"filter"} returns a the middle sub-vector of \code{"open"},
    namely, the result of running a weighted mean of \code{x} with
    weights \code{y}.}
}
\description{
    Use the Fast Fourier Transform to compute the
    several kinds of convolutions of two sequences.
}
\details{
    The Fast Fourier Transform, \code{\link{fft}}, is used for efficiency.

    The input sequences \code{x} and  \code{y} must have the same length if
    \code{circular = TRUE}).
}
\value{
    If \code{r <- convolve(x,y, conj=TRUE, type)}
    and \code{n <- length(x)}, then
    \deqn{r_k = \sum_{i=1}^n x_i y_{k-i}}{r[k] = sum(i=1,..,n;  x[i] * y[k-i])}
    for \eqn{k = 1,\dots,n}.

%%-MM: TODO: Polish --- circular / open / filter
%% --  ----  specify the length of the result for different  `type' !
    If \code{type == "circular"}, then
    \eqn{y_{j} = y_{n+j}}{y[j] == y[n+j]} for \eqn{j < 0}.
}
\references{
Brillinger, D. R. (1981).
\emph{Time Series: Data Analysis and Theory}, Second Edition.
San Francisco: Holden-Day.
}
\seealso{\code{\link{fft}}, \code{\link{nextn}}.
}
\examples{
x <- c(0,0,0,100,0,0,0)
y <- c(0,0,1, 2 ,1,0,0)/4
zapsmall(convolve(x,y))		#  *NOT* what you first thought..
zapsmall(convolve(x, y[3:5], type="f")) # rather
x <- rnorm(50)
y <- rnorm(50)
# Circular convolution *has* this symmetry:
all.equal(convolve(x,y, conj = FALSE),
          rev(convolve(rev(y),x)))

n <- length(x <- -20:24)
y <- (x-10)^2/1000 + rnorm(x)/8

Han <- function(y) # Hanning
       convolve(y, c(1,2,1)/4, type = "filter")

plot(x,y, main="Using  convolve(.) for Hanning filters")
lines(x[-c(1  , n)      ], Han(y), col="red")
lines(x[-c(1:2, (n-1):n)], Han(Han(y)), lwd=2, col="dark blue")
}
\keyword{math}
\keyword{dplot}
