% File src/library/grDevices/man/convertColor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{convertColor}
\alias{convertColor}
\alias{colorspaces}
\title{Convert between Colour Spaces}
\description{
  Convert colours between their representations in standard
  colour spaces.
}
\usage{
convertColor(color, from, to, from.ref.white, to.ref.white,
             scale.in = 1, scale.out = 1, clip = TRUE)
}
\arguments{
  \item{color}{A matrix whose rows specify colors.}
  \item{from, to}{Input and output color spaces.  See \sQuote{Details} below.}
  \item{from.ref.white, to.ref.white}{Reference whites or \code{NULL}
    if these are built in to the definition, as for RGB
    spaces. \code{D65} is the default, see \sQuote{Details} for others.
  }
  \item{scale.in, scale.out}{Input is divided by \code{scale.in}, output
    is multiplied by \code{scale.out}. Use \code{NULL} to suppress
    scaling when input or output is not numeric.}
  \item{clip}{If \code{TRUE}, truncate RGB output to [0,1],
    \code{FALSE} return out-of-range RGB, \code{NA} set out of range
    colors to \code{NaN}.}
}
\details{
  Color spaces are specified by objects of class \code{colorConverter},
  created by \code{\link{colorConverter}} or \code{\link{make.rgb}}.
  Built-in color spaces may be referenced by strings: \code{"XYZ"},
  \code{"sRGB"}, \code{"Apple RGB"}, \code{"CIE RGB"}, \code{"Lab"},
  \code{"Luv"}.  The converters for these colour spaces are in the object
  \code{colorspaces}.

  The \code{"sRGB"} color space is that used by standard PC monitors.
  \code{"Apple RGB"} is used by Apple monitors. \code{"Lab"} and
  \code{"Luv"} are approximately perceptually uniform
  spaces standardized by the Commission Internationale
  d'Eclairage. \code{XYZ} is a 1931 CIE standard capable of representing
  all visible colors (and then some), but not in a perceptually uniform
  way.

  The \code{Lab} and \code{Luv} spaces describe colors of objects, and
  so require the specification of a reference \sQuote{white light}
  color.  Illuminant \code{D65} is a standard indirect daylight,
  Illuminant \code{D50} is close to direct sunlight, and Illuminant
  \code{A} is the light from a standard incandescent bulb. Other
  standard CIE illuminants supported are \code{B}, \code{C}, \code{E} and
  \code{D55}.  RGB colour spaces are defined relative to a particular
  reference white, and can be only approximately translated to other
  reference whites.  The von Kries chromatic adaptation algorithm is used
  for this.  Prior to R 3.6, color conversions involving color spaces
  created with \code{\link{make.rgb}} were carried out assuming a \code{D65}
  illuminant, irrespective of the actual illuminant used in the creation of the
  color space.  This affected the built-in \code{"CIE RGB"} color space.

  The RGB color spaces are specific to a particular class of display.
  An RGB space cannot represent all colors, and the \code{clip} option
  controls what is done to out-of-range colors.
  
  For the named color spaces \code{color} must be a matrix of values in
  the \code{from} color space: in particular opaque colors.
}
\value{
  A 3-column matrix whose rows specify the colors.
}

\references{
  For all the conversion equations \url{http://www.brucelindbloom.com/}.

  For the white points
  \url{http://www.efg2.com/Lab/Graphics/Colors/Chromaticity.htm}.
}

\seealso{
  \code{\link{col2rgb}} and \code{\link{colors}} for ways to specify
  colors in graphics.

  \code{\link{make.rgb}} for specifying other colour spaces.
}
\examples{
## The displayable colors from four planes of Lab space
ab <- as.matrix(expand.grid(a = (-10:15)*10,
                            b = (-15:10)*10))
require(graphics); require(stats) # for na.omit
par(mfrow = c(2, 2), mar = .1+c(3, 3, 3, .5), mgp = c(2,  .8,  0))

Lab <- cbind(L = 20, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
  xlab = "a", ylab = "b", main = "Lab: L=20")

Lab <- cbind(L = 40, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
  xlab = "a", ylab = "b", main = "Lab: L=40")

Lab <- cbind(L = 60, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
  xlab = "a", ylab = "b", main = "Lab: L=60")

Lab <- cbind(L = 80, ab)
srgb <- convertColor(Lab, from = "Lab", to = "sRGB", clip = NA)
clipped <- attr(na.omit(srgb), "na.action")
srgb[clipped, ] <- 0
cols <- rgb(srgb[, 1], srgb[, 2], srgb[, 3])
image((-10:15)*10, (-15:10)*10, matrix(1:(26*26), ncol = 26), col = cols,
  xlab = "a", ylab = "b", main = "Lab: L=80")

cols <- t(col2rgb(palette())); rownames(cols) <- palette(); cols
zapsmall(lab <- convertColor(cols, from = "sRGB", to = "Lab", scale.in = 255))
stopifnot(all.equal(cols, # converting back.. getting the original:
   round(convertColor(lab, from = "Lab", to = "sRGB", scale.out = 255)),
                    check.attributes = FALSE))
}
\keyword{color}
