% File src/library/grDevices/man/rgb.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{rgb}
\title{RGB Color Specification}
\usage{
rgb(red, green, blue, alpha, names = NULL, maxColorValue = 1)
}
\alias{rgb}
\description{
  This function creates colors corresponding to the given
  intensities (between 0 and \code{max}) of the red, green and blue
  primaries.  The colour specification refers to the
  standard sRGB colorspace (IEC standard 61966).

  An alpha transparency value can also be specified (as an opacity, so
  \code{0} means fully transparent and \code{max} means opaque).  If
  \code{alpha} is not specified, an opaque colour is generated.

  The \code{names} argument may be used to provide names for the
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
  The colors may be specified by passing a matrix or data frame as argument
  \code{red}, and leaving \code{blue} and \code{green} missing.
  In this case the first three columns of \code{red} are taken to be
  the \code{red}, \code{green} and \code{blue} values.

  Semi-transparent colors (\code{0 < alpha < 1}) are supported only on
  some devices: at the time of writing on the \code{\link{pdf}},
  \code{windows}, \code{quartz} and \code{X11(type = "cairo")} devices and
  associated bitmap devices (\code{jpeg}, \code{png}, \code{bmp},
  \code{tiff} and \code{bitmap}). They are supported by several
  third-party devices such as those in packages \CRANpkg{Cairo},
  \CRANpkg{cairoDevice} and \CRANpkg{JavaGD}.  Only some of these
  devices support semi-transparent backgrounds.

  Most other graphics devices plot semi-transparent colors as fully
  transparent, usually with a warning when first encountered.
}
\value{
  A character vector with elements of 7 or 9 characters, \code{"#"}
  followed by the red, blue, green and optionally alpha values in
  hexadecimal (after rescaling to \code{0 ... 255}).  The optional alpha
  values range from \code{0} (fully transparent) to \code{255} (opaque).

  \R does \strong{not} use \sQuote{premultiplied alpha}.
}
\seealso{
  \code{\link{col2rgb}} for translating \R colors to RGB vectors;
  \code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{hcl}},
  \code{\link{gray}}.
}
\examples{
rgb(0, 1, 0)

rgb((0:15)/15, green = 0, blue = 0, names = paste("red", 0:15, sep = "."))

rgb(0, 0:12, 0, max = 255) # integer input

ramp <- colorRamp(c("red", "white"))
rgb( ramp(seq(0, 1, length = 5)), max = 255)
}
\keyword{color}
