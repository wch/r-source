\name{randu}
\alias{randu}
\title{Random Numbers from Congruential Generator}
\usage{data(randu)}
\description{
    400 Triplets of successive random numbers were taken from the VAX
    FORTRAN function RANDU running under VMS 1.5.}
\details{
    In three dimensional displays it is evident that the numbers fall in
    15 parallel planes in 3-space.

    Under VMS versions 2.0 and higher, this problem has been fixed.
}
\format{
    A data frame with 400 observations on 3 variables named \code{x},
    \code{y} and \code{z} which give the first, second and third random
    number in the triplet.
}
\source{
    David Donoho
}
\keyword{datasets}
