\name{convolve}
\title{Fast Convolution}
\usage{
convolve(x, y, conj = TRUE)
}
\alias{convolve}
\arguments{
\item{x,y}{numeric sequences \emph{of the same length} to be convolved.}
}
\value{
This function uses the Fast Fourier Transform to compute the
(circular) convolution of the sequences given as its arguments.
If \code{r <- convolve(x,y)} and \code{n <- length(x)}, then
\deqn{r_k = \sum_{i=1}^n x_i y_{k-i}}{r[k] = sum(i=1,..,n;  x[i] * y[k-i])}
for \eqn{k = 1,\dots,n}, where \eqn{y_{j} = y_{n+j}}{y[j] == y[n+j]} for
\eqn{j < 0}.

The Fast Fourier Transform, \code{\link{fft}}, is used for efficiency.
}
\references{
Brillinger, D. R. (1981).
\emph{Time Series: Data Analysis and Theory}, Second Edition.
San Francisco: Holden-Day.
}
\seealso{
\code{\link{fft}}, \code{\link{nextn}}.
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
