\name{Trig}
\title{Trigonometric Functions}
\usage{
cos(x)
sin(x)
tan(x)
acos(x)
asin(x)
atan(x)
atan2(y, x)
}
\alias{cos}
\alias{sin}
\alias{tan}
\alias{acos}
\alias{asin}
\alias{atan}
\alias{atan2}
\description{
These functions give the obvious trigonometric functions.  They
respectively compute the cosine, sine, tangent, arc-cosine, arc-sine,
arc-tangent and arc-tangent of two arguments.
}
\seealso{
\code{\link{Log}}, \code{\link{Math}}.
}
\examples{
cos(0) == 1
sin(3*pi/2) == cos(pi)
x <- rnorm(99)
all( sin(-x) == - sin(x))
all( cos(-x) == cos(x))

x <- 1:99/100
all(Mod(1 - (cos(x) + 1i*sin(x)) / exp(1i*x)) < 1.1 * .Machine$double.eps)
 2* abs(1 - x / acos(cos(x))) / .Machine$double.eps #-- depends ?
all(abs(1 - x / asin(sin(x))) <= .Machine$double.eps) # TRUE
all(abs(1 - x / atan(tan(x))) <= .Machine$double.eps) # TRUE
}
\keyword{math}
