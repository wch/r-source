\name{png}
\alias{bmp}
\alias{png}
\alias{jpeg}
\title{BMP, JPEG and PNG graphics devices}
\description{
  A graphics device for BMP, JPEG or PNG format bitmap files.
}
\usage{
bmp(filename="Rplot.bmp", width=480, height=480, pointsize=12)
jpeg(filename="Rplot.jpg", width=480, height=480, pointsize=12, quality=75)
png(filename="Rplot.png", width=480, height=480, pointsize=12)
}
\arguments{
 \item{filename}{the name of the output file.}
 \item{width}{the width of the device in pixels.}
 \item{height}{the height of the device in pixels.}
 \item{pointsize}{the default pointsize of plotted text, intepreted at
   72 dpi, so one point is approximately one pixel.}
 \item{quality}{the `quality' of the JPEG image, as a
   percentage. Smaller values will give more compression but also more
   degradation of the image.}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in most modern web
  browsers. The PNG format is lossless and is best for line
  diagrams and blocks of solid colour. The JPEG format is lossy,
  but may be useful for image plots, for example. The BMP format is
  standard on Windows, and supported elsewhere.
}
\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}

\author{Guido Masarotto}

\seealso{\code{\link{Devices}}, \code{\link{dev.print}}}
\examples{
## copy current plot to a (large) PNG file
\dontrun{dev.print(png, file="myplot.png", width=1024, height=768}
}
\keyword{device}
