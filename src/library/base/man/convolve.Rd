\name{convolve}
\title{Fast Convolution}
\usage{
convolve(x, y, conj = TRUE, circular = TRUE)
}
\alias{convolve}
\arguments{
\item{x,y}{numeric sequences \emph{of the same length} to be convolved.}
\item{conj}{logical; if \code{TRUE}, take the complex \emph{conjugate}
    before back-transforming (default, and used for usual convolution).}
\item{circular}{logical; if \code{TRUE}, the two sequences are treated
    as \emph{circular}, i.e., periodic.  Otherwise (\code{circular =
	FALSE}), they are padded with \code{0}s first.}
}
\description{
    Use the Fast Fourier Transform to compute the
    (circular) convolution of two sequences.
}
\details{
    The Fast Fourier Transform, \code{\link{fft}}, is used for efficiency.

    The input sequences \code{x} and  \code{y} must have the same length if
    \code{circular = TRUE}).

    The usual ``open'' convolution uses \code{circular = FALSE}.
    It can be used for a fast weighted running mean filter.
}
\value{
%%-MM: TODO: Polish --- circular  <-> non-circular
%% --  Show how to use for weighted running mean filter !
    If \code{r <- convolve(x,y, conj=TRUE, circular= Circ )}
    and \code{n <- length(x)}, then
    \deqn{r_k = \sum_{i=1}^n x_i y_{k-i}}{r[k] = sum(i=1,..,n;  x[i] * y[k-i])}
    for \eqn{k = 1,\dots,n}.

    If \code{Circ = TRUE}, then
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
round(convolve(x,y), 7)# this is *NOT* what you first thought..
x <- rnorm(50);y <- rnorm(50)
all(convolve(x,y), convolve(y,x))
all(convolve(x,y, conj = FALSE), rev(convolve(y,x)))
}
\keyword{math}
\keyword{dplot}
