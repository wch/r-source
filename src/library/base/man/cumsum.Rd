\name{cumsum}
\title{Cumulative Sums, Products, etc.}
\usage{
cumsum(x)
cumprod(x)
cummax(x)
cummin(x)
}
\alias{cumsum}
\alias{cumprod}
\alias{cummin}
\alias{cummax}
\value{
A vector with the same length of \code{x},
but whose elements are the cumulative sums,
products, minima or maxima of those in \code{x}.
An \code{NA} value in \code{x} will the corresponding
and following elements of the value to be \code{NA}.
}
\examples{
cumsum(1:10)
cumprod(1:10)
cummin(c(3:1,2:0,4:2))
cummax(c(3:1,2:0,4:2))
}
\keyword{arith}
