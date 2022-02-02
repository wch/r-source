% File src/library/grDevices/man/make.rgb.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{make.rgb}
\alias{make.rgb}
\alias{colorConverter}
\title{Create colour spaces }
\description{
  These functions specify colour spaces for use in
  \code{\link{convertColor}}.
}
\usage{
make.rgb(red, green, blue, name = NULL, white = "D65",
         gamma = 2.2)

colorConverter(toXYZ, fromXYZ, name, white = NULL, vectorized = FALSE)
}
\arguments{
  \item{red,green,blue}{Chromaticity (xy or xyY) of RGB primaries}
  \item{name}{Name for the colour space}
  \item{white}{Character string specifying the reference white (see
    \sQuote{Details}.)}
  \item{gamma}{Display gamma (nonlinearity). A positive number or the
    string \code{"sRGB"} }
  \item{fromXYZ}{Function to convert from XYZ tristimulus coordinates
    to this space}
  \item{toXYZ}{Function to convert from this space to XYZ tristimulus
    coordinates.}
  \item{vectorized}{Whether \code{fromXYZ} and \code{toXYZ} are
    vectorized internally to handle input color matrices.}
}
\details{
  An RGB colour space is defined by the chromaticities of the  red, green and
  blue primaries.  These are given as vectors of length 2 or 3 in xyY
  coordinates (the Y component is not used and may be omitted).
  The chromaticities are defined relative to a reference white, which
  must be one of the CIE standard illuminants: "A", "B", "C",
  "D50", "D55", "D60", "E" (usually "D65").

  The display gamma is most commonly 2.2, though 1.8 is used for Apple RGB.
  The sRGB standard specifies a more complicated function that is close
  to a gamma of 2.2; \code{gamma = "sRGB"} uses this function.

  Colour spaces other than RGB can be specified directly by giving
  conversions to and from XYZ tristimulus coordinates.
  The functions  should take two arguments. The first is a vector giving the
  coordinates for one colour. The second argument is the reference
  white. If a specific reference white is included in the definition of
  the colour space (as for the RGB spaces) this second argument should
  be ignored and may be \code{...}.

  As of R 3.6.0 the built in color converters along with
  \code{\link{convertColor}} were vectorized to process three column
  color matrices in one call, instead of row by row via \code{\link{apply}}.
  In order to maintain backwards compatibility, \code{colorConverter} wraps
  \code{fromXYZ} and \code{toXYZ} in a \code{apply} loop in case they do not
  also support matrix inputs.  If the \code{fromXYZ} and \code{toXYZ} functions
  you are using operate correctly on the whole color matrix at once instead of
  row by row, you can set \code{vectorized=TRUE} for a performance improvement.
}
\value{
  An object of class \code{colorConverter}
}
\references{
  Conversion algorithms from \url{http://www.brucelindbloom.com}.
}
\seealso{
  \code{\link{convertColor}
  }
}
\examples{
(pal <- make.rgb(red =   c(0.6400, 0.3300),
                 green = c(0.2900, 0.6000),
                 blue =  c(0.1500, 0.0600),
                 name = "PAL/SECAM RGB"))

## converter for sRGB in #rrggbb format
hexcolor <- colorConverter(toXYZ = function(hex, ...) {
                            rgb <- t(col2rgb(hex))/255
                            colorspaces$sRGB$toXYZ(rgb, ...) },
                           fromXYZ = function(xyz, ...) {
                              rgb <- colorspaces$sRGB$fromXYZ(xyz, ...)
                              rgb <- round(rgb, 5)
                              if (min(rgb) < 0 || max(rgb) > 1)
                                   as.character(NA)
                              else rgb(rgb[1], rgb[2], rgb[3])},
                           white = "D65", name = "#rrggbb")

(cols <- t(col2rgb(palette())))
zapsmall(luv <- convertColor(cols, from = "sRGB", to = "Luv", scale.in = 255))
(hex <- convertColor(luv, from = "Luv",  to = hexcolor, scale.out = NULL))

## must make hex a matrix before using it
(cc <- round(convertColor(as.matrix(hex), from = hexcolor, to = "sRGB",
                          scale.in = NULL, scale.out = 255)))
stopifnot(cc == cols)

## Internally vectorized version of hexcolor, notice the use
## of `vectorized = TRUE`:

hexcolorv <- colorConverter(toXYZ = function(hex, ...) {
                            rgb <- t(col2rgb(hex))/255
                            colorspaces$sRGB$toXYZ(rgb, ...) },
                           fromXYZ = function(xyz, ...) {
                              rgb <- colorspaces$sRGB$fromXYZ(xyz, ...)
                              rgb <- round(rgb, 5)
                              oob <- pmin(rgb[,1],rgb[,2],rgb[,3]) < 0 |
                                     pmax(rgb[,1],rgb[,2],rgb[,3]) > 0
                              res <- rep(NA_character_, nrow(rgb))
                              res[!oob] <- rgb(rgb[!oob,,drop=FALSE])},
                           white = "D65", name = "#rrggbb",
                           vectorized=TRUE)
(ccv <- round(convertColor(as.matrix(hex), from = hexcolor, to = "sRGB",
                           scale.in = NULL, scale.out = 255)))
stopifnot(ccv == cols)

}
\keyword{color}

