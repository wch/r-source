\name{abs}
\title{Miscellaneous Mathematical Functions}
\usage{
abs(x)
sqrt(x)
}
\alias{abs}
\alias{sqrt}
\description{
These functions compute miscellaneous mathematical functions.
The naming follows the standard for computer languages
such as C or Fortran.
}
\seealso{
\code{\link{Arithmetic}} for simple, \code{\link{log}} for logarithmic,
\code{\link{sin}} for trigonometric, and \code{\link{Special}} for
special mathematical functions.
}
\examples{
xx <- -9:9
plot(xx, sqrt(abs(xx)),  col = "red")
lines(spline(xx, sqrt(abs(xx)), n=101), col = "pink")
}
\keyword{math}
