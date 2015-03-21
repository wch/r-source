% File src/library/stats/man/convolve.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{convolve}
\alias{convolve}
\title{Convolution of Sequences via FFT}
\description{
  Use the Fast Fourier Transform to compute the several kinds of
  convolutions of two sequences.
}
\usage{
convolve(x, y, conj = TRUE, type = c("circular", "open", "filter"))
}
\arguments{
  \item{x, y}{numeric sequences \emph{of the same length} to be
    convolved.}
  \item{conj}{logical; if \code{TRUE}, take the complex \emph{conjugate}
    before back-transforming (default, and used for usual convolution).}
  \item{type}{character; partially matched to \code{"circular"}, \code{"open"},
    \code{"filter"}.  For \code{"circular"}, the
    two sequences are treated as \emph{circular}, i.e., periodic.

    For \code{"open"} and \code{"filter"}, the sequences are padded with
    \code{0}s (from left and right) first; \code{"filter"} returns the
    middle sub-vector of \code{"open"}, namely, the result of running a
    weighted mean of \code{x} with weights \code{y}.}
}
\details{
  The Fast Fourier Transform, \code{\link{fft}}, is used for efficiency.

  The input sequences \code{x} and  \code{y} must have the same length if
  \code{circular} is true.

  Note that the usual definition of convolution of two sequences
  \code{x} and \code{y} is given by \code{convolve(x, rev(y), type = "o")}.
}
\value{
  If \code{r <- convolve(x, y, type = "open")}
  and \code{n <- length(x)}, \code{m <- length(y)}, then
  \deqn{r_k = \sum_{i} x_{k-m+i} y_{i}}{r[k] = sum(i; x[k-m+i] * y[i])}
  where the sum is over all valid indices \eqn{i}, for
  \eqn{k = 1, \dots, n+m-1}.

  If \code{type == "circular"}, \eqn{n = m} is required, and the above is
  true for \eqn{i , k = 1,\dots,n} when
  \eqn{x_{j} := x_{n+j}}{x[j] := x[n+j]} for \eqn{j < 1}.
}
\references{
  Brillinger, D. R. (1981)
  \emph{Time Series: Data Analysis and Theory}, Second Edition.
  San Francisco: Holden-Day.
}
\seealso{\code{\link{fft}}, \code{\link{nextn}}, and particularly
  \code{\link{filter}} (from the \pkg{stats} package) which may be
  more appropriate.
}
\examples{
require(graphics)

x <- c(0,0,0,100,0,0,0)
y <- c(0,0,1, 2 ,1,0,0)/4
zapsmall(convolve(x, y))         #  *NOT* what you first thought.
zapsmall(convolve(x, y[3:5], type = "f")) # rather
x <- rnorm(50)
y <- rnorm(50)
# Circular convolution *has* this symmetry:
all.equal(convolve(x, y, conj = FALSE), rev(convolve(rev(y),x)))

n <- length(x <- -20:24)
y <- (x-10)^2/1000 + rnorm(x)/8

Han <- function(y) # Hanning
       convolve(y, c(1,2,1)/4, type = "filter")

plot(x, y, main = "Using  convolve(.) for Hanning filters")
lines(x[-c(1  , n)      ], Han(y), col = "red")
lines(x[-c(1:2, (n-1):n)], Han(Han(y)), lwd = 2, col = "dark blue")
}
\keyword{math}
\keyword{dplot}
