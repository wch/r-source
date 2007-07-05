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
  \item{red, blue, green, alpha}{numeric vectors with values in \eqn{[0, M]}
    where \eqn{M} is \code{maxColorValue}.  When this is \code{255}, the
    \code{red}, \code{blue}, \code{green}, and \code{alpha}
    values are coerced to integers in \code{0:255} and the result is
    computed most efficiently.}
  \item{names}{character. The names for the resulting vector.}
  \item{maxColorValue}{number giving the maximum of the color values
    range, see above.}
}
\details{
  The colors may be specified by passing a matrix or dataframe as argument
  \code{red}, and leaving \code{blue} and \code{green} missing.
  In this case the first three columns of \code{red} are taken to be
  the \code{red}, \code{green} and \code{blue} values.

  Semi-transparent colors (\code{0 < alpha < 1}) are supported only on
  some devices: at the time of writing only on the \code{\link{pdf}} and
  (on MacOS X) \code{quartz} devices as well as several third-party
  devices such as those in packages \pkg{Cairo}, \pkg{cairoDevice},
  \pkg{JavaGD} and \pkg{RSvgDevice}.
}
\value{
  A character vector with elements of 7 or 9 characters, \code{"#"}
  followed by the red, blue, green and optionally alpha values in
  hexadecimal (after rescaling to \code{0 ... 255}).
}
\seealso{
  \code{\link{col2rgb}} the \dQuote{inverse} for translating \R colors
  to RGB vectors;
  \code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{hcl}},
  \code{\link{gray}}.
}
\examples{
rgb(0,1,0)
(u01 <- seq(0,1, length=11))
stopifnot(rgb(u01,u01,u01) == gray(u01))
reds <- rgb((0:15)/15, g=0,b=0, names=paste("red",0:15,sep="."))
reds

rgb(0, 0:12, 0, max = 255)# integer input

ramp <- colorRamp(c("red", "white"))
rgb( ramp(seq(0, 1, length = 5)), max = 255)
}
\keyword{color}
