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
arc-tangent, and the two-argument arc-tangent. 
}
\details{
    The arc-tangent of two arguments \code{atan2(y,x)} returns the angle
    between the x-axis and the vector from the origin to \eqn{(x,y)}, i.e., for
    positive arguments \code{atan2(y,x) == atan(y/x)}.

    Angles are in radians, not degrees (i.e. a right angle is \eqn{\pi/2}).
}
\seealso{
\code{\link{Log}}, \code{\link{Math}}.
}
\examples{
cos(0) == 1
sin(3*pi/2) == cos(pi)
x <- rnorm(99)
all.equal( sin(-x), - sin(x))
all.equal( cos(-x), cos(x))
x <- abs(x); y <- abs(rnorm(x))
all(abs(atan2(y, x) - atan(y/x)) <= .Machine$double.eps)# TRUE
table(abs(atan2(y, x) - atan(y/x)) / .Machine$double.eps) # depends!

x <- 1:99/100
all(Mod(1 - (cos(x) + 1i*sin(x)) / exp(1i*x)) < 1.1 * .Machine$double.eps)
 2* abs(1 - x / acos(cos(x))) / .Machine$double.eps #-- depends ?
all(abs(1 - x / asin(sin(x))) <= .Machine$double.eps) # TRUE
all(abs(1 - x / atan(tan(x))) <= .Machine$double.eps) # TRUE
}
\keyword{math}
