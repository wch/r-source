\name{convolve}
\title{Fast Convolution}
\usage{
convolve(a, b)
}
\alias{convolve}
\arguments{
\item{a,b}{the sequences to be convolved.}
}
\value{
This function uses the Fast Fourier Transform to compute the
(circular) convolution of the sequences given as its arguments.
}
\references{
Brillinger, D. R. (1981).
\emph{Time Series: Data Analysis and Theory}, Second Edition.
San Francisco: Holden-Day.
}
\seealso{
\code{\link{fft}}, \code{\link{nextn}}.
}
\keyword{math}
\keyword{dplot}
