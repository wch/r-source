\name{Extremes}
\title{Maxima and Minima}
\usage{
max(\dots, na.rm=FALSE)
min(\dots, na.rm=FALSE)

pmax(\dots, na.rm=FALSE)
pmin(\dots, na.rm=FALSE)
}
\alias{max}
\alias{min}
\alias{pmax}
\alias{pmin}
\value{
\code{max} and \code{min} return the maximum or minimum of all the values
present in their arguments, as \code{\link{double}}.
If \code{na.rm} is \code{FALSE} an \code{NA} value in any
of the arguments will cause a value of \code{NA} to be returned,
otherwise \code{NA} values are ignored.

\code{pmax} and \code{pmin} take several vectors as arguments and return a
single vector giving the parallel maxima (or minima) of the vectors.
The first element of the result is the maximum (minimum) of the first
elements of all the arguments, the second element of the result is the
maximum (minimum) of the second elements of all the arguments and so on.
Shorter vectors are recycled if necessary.  If \code{na.rm} is \code{FALSE},
\code{NA} values in the input vectors will produce \code{NA} values in the
output.  If \code{na.rm} is \code{TRUE}, \code{NA} values are ignored.
}
\seealso{
\code{\link{range}}.
}
\examples{
min(5:1,pi)
pmin(5:1, pi)
x <- sort(rnorm(100));  cH <- 1.35
plot(x, pmin(cH, pmax(-cH, x)), type='b', main= "Huber's function")
}
\keyword{univar}
\keyword{arith}
