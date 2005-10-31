\name{rgb}
\title{RGB Color Specification}
\usage{
rgb(red, green, blue, alpha, names = NULL, maxColorValue = 1)
}
\alias{rgb}
\description{
  This function creates \dQuote{colors} corresponding to the given
  intensities (between 0 and \code{max}) of the red, green and blue
  primaries.

  An alpha transparency value can also be specified
  (0 means fully transparent and \code{max} means opaque).
  If \code{alpha} is not specified, an opaque colour is generated.

  The names argument may be used to provide names for the
  colors.
  
  The values returned by these functions can be used with a
  \code{col=} specification in graphics functions or in \code{\link{par}}.
}
\arguments{
  \item{red, blue, green, alpha}{vectors of same length
    with values in \eqn{[0, M]}
    where \eqn{M} is \code{maxColorValue}. When this is \code{255}, the
    \code{red}, \code{blue}, \code{green}, and \code{alpha}
    values are coerced to
    integers in \code{0:255} and the result is computed most efficiently.}
  \item{names}{character. The names for the resulting vector.}
  \item{maxColorValue}{number giving the maximum of the color values
    range, see above.}
}
\seealso{
  \code{\link{col2rgb}} the \dQuote{inverse} for translating \R colors
  to RGB vectors;
  \code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{hcl}}, \code{\link{gray}}.
}
\examples{
rgb(0,1,0)
(u01 <- seq(0,1, length=11))
stopifnot(rgb(u01,u01,u01) == gray(u01))
reds <- rgb((0:15)/15, g=0,b=0, names=paste("red",0:15,sep="."))
reds

rgb(0, 0:12, 0, max = 255)# integer input
}
\keyword{color}
