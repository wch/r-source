\name{make.rgb}
\alias{make.rgb}
\alias{colorConverter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create colour spaces }
\description{
These functions specify colour spaces for use in \code{\link{convertColor}}
}
\usage{
make.rgb(red, green, blue, name = NULL, white = "D65", gamma = 2.2)
colorConverter(toXYZ, fromXYZ, name, white=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{red,green,blue}{Chromaticity (xy or xyY) of RGB primaries}
  \item{name}{Name for the colour space}
  \item{white}{Character string specifying the reference white (see Details)}
  \item{gamma}{Display gamma (nonlinearity). A positive number or the
    string \code{"sRGB"} }
  \item{fromXYZ}{Function to convert from XYZ tristimulus coordinates
    to this space}
  \item{toXYZ}{Function to convert from this space to XYZ tristimulus coordinates}
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
  to a gamma of 2.2; \code{gamma="sRGB"} uses this function.

  Colour spaces other than RGB can be specified directly by giving
  conversions to and from XYZ tristimulus coordinates. 
  The functions  should take two arguments. The first is a vector giving the
  coordinates for one colour. The second argument is the reference
  white. If a specific reference white is included in the definition of
  the colour space (as for the RGB spaces) this second argument should
  be ignored and may be \code{...}.  
   
}
\value{
  An object of class \code{colorConverter}
}
\references{Conversion algorithms from \url{http://www.brucelindbloom.com}}

\seealso{\code{\link{convertColor}}}
\examples{
(pal<-make.rgb(red=c(0.6400,0.3300),
              green=c(0.2900,0.6000),
              blue=c(0.1500,0.0600),
              name="PAL/SECAM RGB"))

## converter for sRGB in #rrggbb format
hexcolor<-colorConverter(toXYZ=function(hex,...) {
                            rgb<-t(col2rgb(hex))/255
                            colorspaces$sRGB$toXYZ(rgb,...)},
                         fromXYZ=function(xyz,...){
                            rgb<-colorspaces$sRGB$fromXYZ(xyz,..)
                            rgb<-round(rgb,5)
                            if (min(rgb)<0 || max(rgb)>1)
                                 as.character(NA)
                            else
                                 rgb(rgb[1],rgb[2],rgb[3])},
                         white="D65", name="#rrggbb")

(cols<-t(col2rgb(palette())))
(luv<-convertColor(cols,from="sRGB",to="Luv",scale.in=255))
(hex<-convertColor(luv,from="Luv", to=hexcolor, scale.out=NULL))

## must make hex a matrix before using it
round(convertColor(as.matrix(hex), from=hexcolor,
         to="sRGB", scale.in=NULL,scale.out=255))

                        
}
\keyword{color}% at least one, from doc/KEYWORDS

