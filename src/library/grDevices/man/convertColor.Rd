\name{convertColor}
\alias{convertColor}
\title{Convert between color spaces}
\description{
  Convert colors between standard color space representations. This
  function is experimental, and may be made obsolete by changes to \R
  color handling.
}
\usage{
convertColor(color,
             from = c("sRGB", "hexsRGB","XYZ", "Lab", "Luv", "AppleRGB",
                      "AdobeRGB", "CIE.RGB", "NTSC.RGB"),
             to = c("sRGB", "hexsRGB","XYZ", "Lab", "Luv", "AppleRGB",
                    "AdobeRGB", "CIE.RGB", "NTSC.RGB"),
             white.point = c("D65", "A", "B", "C", "D50", "D55", "E"),
             gamma = 2.2, scale = 1, clip = TRUE)
}
\arguments{
  \item{color}{A 3-column matrix whose rows specify colors (or a vector
    of strings for \code{hexsRGB}.)}
  \item{from}{Input color space.}
  \item{to}{Output color space.}
  \item{white.point}{Reference white for \code{"Lab"} and \code{"Luv"}
    spaces.}
  \item{gamma}{Display gamma for \code{RGB} spaces.}
  \item{scale}{Input color components are divided by this number.
    (E.g., 255 for 8-bit RGB.)}
  \item{clip}{If \code{TRUE}, truncate RGB output to [0,1],
    \code{FALSE} return out-of-range RGB, \code{NA} set out of range
    colors to \code{NaN}.}
}
\details{
  The \code{sRGB} color space is that used by standard PC monitors.
  \code{AppleRGB} is used by Apple monitors, with a gamma of about
  1.8. \code{Lab} and \code{Luv} are approximately perceptually uniform
  spaces standardized by the Commission Internationale
  d'Eclairage. \code{XYZ} is a 1931 CIE standard capable of representing
  all visible colors (and then some), but not in a perceptually uniform
  way.
  
  The \code{Lab} and \code{Luv} spaces describe colors of objects, and
  so require the specification of a reference \dQuote{white light}
  color.  Illuminant \code{D65} is a standard indirect daylight,
  Illuminant \code{D50} is close to direct sunlight, and Illuminant
  \code{A} is the light from a standard incandescent bulb.
  
  The RGB color spaces are specific to a particular class of
  display. They require the user to specify the display gamma, which
  describes the nonlinearity of the display response. The exception is
  \code{sRGB}, where the nonlinearity is specified by the standard and
  corresponds approximately to \code{gamma=2.2}. An RGB space cannot
  represent all colors, and the \code{clip} option controls what is
  done to out-of-range colors.

  \code{hexsRGB} specifies the \code{sRGB} color space and that the
  colors are represented as strings \code{#RRGGBB} rather than matrices.
}  
\value{
  A 3-row matrix whose columns specify the colors.
}

\note{The RGB spaces need to have their white points fixed up.  They
  work at D50}

  
\references{
  For the conversion equations
  \url{http://www.brucelindbloom.com/index.html}
  
  For the white points
  \url{http://www.efg2.com/Lab/Graphics/Colors/Chromaticity.htm}
}

\seealso{
  \code{\link{col2rgb}},
  \code{\link{colors}}
}
\examples{

par(mfrow=c(2,2))
## The displayable colors from four planes of Lab space
ab <- expand.grid(a=(-10:15)*10,b=(-15:10)*10)

Lab <- cbind(L=20,ab)
srgb <- convertColor(Lab,from="Lab",to="sRGB",clip=NA)
clipped <- attr(na.omit(srgb),"na.action")
srgb[clipped,] <- 0
cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
image((-10:15)*10,(-15:10)*10,matrix(1:(26*26),ncol=26),col=cols,
  xlab="a",ylab="b",main="Lab: L=20")

Lab <- cbind(L=40,ab)
srgb <- convertColor(Lab,from="Lab",to="sRGB",clip=NA)
clipped <- attr(na.omit(srgb),"na.action")
srgb[clipped,] <- 0
cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
image((-10:15)*10,(-15:10)*10,matrix(1:(26*26),ncol=26),col=cols,
  xlab="a",ylab="b",main="Lab: L=40")

Lab <- cbind(L=60,ab)
srgb <- convertColor(Lab,from="Lab",to="sRGB",clip=NA)
clipped <- attr(na.omit(srgb),"na.action")
srgb[clipped,] <- 0
cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
image((-10:15)*10,(-15:10)*10,matrix(1:(26*26),ncol=26),col=cols,
  xlab="a",ylab="b",main="Lab: L=60")

Lab <- cbind(L=80,ab)
srgb <- convertColor(Lab,from="Lab",to="sRGB",clip=NA)
clipped <- attr(na.omit(srgb),"na.action")
srgb[clipped,] <- 0
cols <- rgb(srgb[,1],srgb[,2],srgb[,3])
image((-10:15)*10,(-15:10)*10,matrix(1:(26*26),ncol=26),col=cols,
  xlab="a",ylab="b",main="Lab: L=80")


(cols<-t(col2rgb(palette())))
(lab<-convertColor(cols,from="sRGB",to="Lab",scale=256))
round(256*convertColor(lab,from="Lab",to="sRGB"))

}
\keyword{color}
