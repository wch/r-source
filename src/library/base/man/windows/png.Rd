\name{png}
\alias{bmp}
\alias{png}
\alias{jpeg}
\title{BMP, JPEG and PNG graphics devices}
\description{
  A graphics device for BMP, JPEG or PNG format bitmap files.
}
\usage{
bmp(filename = "Rplot\%03d.bmp", width = 480, height = 480, pointsize = 12,
    bg = "white")
jpeg(filename = "Rplot\%03d.jpg", width = 480, height = 480, pointsize = 12,
     quality = 75, bg = "white")
png(filename = "Rplot\%03d.png", width = 480, height = 480, pointsize = 12,
    bg = "white")
}
\arguments{
 \item{filename}{the name of the output file.  The page number is
   substituted if an integer format is included in the character string.}
 \item{width}{the width of the device in pixels.}
 \item{height}{the height of the device in pixels.}
 \item{pointsize}{the default pointsize of plotted text, intepreted at
   72 dpi, so one point is approximately one pixel.}
 \item{bg}{the initial background colour: can be overridden by setting
   par("bg").} 
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

  \code{png} supports transparent backgrounds on 16-bit (`High Color') or
  better screens: use \code{bg = "transparent"}.
  Not all PNG viewers render files with transparency correctly.
}
\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}
\section{Warning}{
  If you plot more than one page on one of these devices and do not
  include somthing like \code{\%d} for the sequence number in
  \code{file}, the file will contain the last page plotted.
}
\note{
  These devices effectively plot on a hidden screen and then copy the
  image to the required format.  This means that they have the same
  colour handling as the actual screen device, and work best if that is
  set to a 24-bit or 32-bit colour mode.
}

\seealso{\code{\link{Devices}}, \code{\link{dev.print}},
  \code{\link{bitmap}}}
\examples{
## copy current plot to a (large) PNG file
\dontrun{dev.print(png, file="myplot.png", width=1024, height=768)

png(file="myplot.png", bg="transparent")
plot(1:10)
rect(1, 5, 3, 7, col="white")
dev.off()

jpeg(file="myplot.jpeg")
example(rect)
dev.off()
}}
\keyword{device}
