\name{png}
\alias{png}
\alias{jpeg}
\title{JPEG and PNG graphics devices}
\description{
  A graphics device for JPEG or PNG format bitmap files.
}
\usage{
jpeg(filename = "Rplot.jpeg", width = 480, height = 480,
     pointsize = 12, quality = 75, bg = "white", \dots)
png(filename ="Rplot.png", width = 480, height = 480,
    pointsize = 12, bg = "white", \dots)
}
\synopsis{
jpeg(filename = "Rplot.jpeg", width = 480, height = 480, pointsize = 12,
     quality = 75, gamma = 1,
     colortype = getOption("X11colortype"), maxcubesize = 256, bg = "white")
png(filename ="Rplot.png", width = 480, height = 480, pointsize = 12,
    gamma = 1, colortype = getOption("X11colortype"), maxcubesize = 256,
    bg = "white")
}
\arguments{
 \item{filename}{the name of the output file.}
 \item{width}{the width of the device in pixels.}
 \item{height}{the height of the device in pixels.}
 \item{pointsize}{the default pointsize of plotted text.}
 \item{quality}{the `quality' of the JPEG image, as a
   percentage. Smaller values will give more compression but also more
   degradation of the image.}
 \item{bg}{default background colour.}
 \item{\dots}{additional arguments to the \code{\link{X11}} device.}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in most modern web
  browsers. The PNG format is lossless and is best for line
  diagrams and blocks of solid colour. The JPEG format is lossy,
  but may be useful for image plots, for example.

  \code{png} supports transparent backgrounds: use \code{bg = "transparent"}.
  Not all PNG viewers render files with transparency correctly.
  When transparency is in use a very light grey is used as the
  background and so will appear as transparent if used in the plot.
  This allows opaque white to be used, as on the example.

  \R can be compiled without support for either or both of these
  devices: this will be reported if you attempt to use them on a system
  where they are not supported.  They will not be not available if \R has
  been started with \code{--gui=none} (and will give a different error
  message).
}
\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}
\note{
  These are based on the \code{\link{X11}} device, so the additional
  arguments to that device work, but are rarely appropriate.  The
  colour handling will be that of the \code{X11} device in use.
}
\author{Guido Masarotto and Brian Ripley}

\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}, \code{\link{bitmap}}.

  \code{\link{capabilities}} to see if these devices are
  supported by this build of \R.
}
\examples{
## copy current plot to a PNG file
\dontrun{dev.print(png, file="myplot.png", width=480, height=480)}
if(capabilities("png")) {
png(file="myplot.png", bg="transparent")
plot(1:10)
rect(1, 5, 3, 7, col="white")
dev.off()
unlink("myplot.png")
}
if(capabilities("jpeg")) {
jpeg(file="myplot.jpeg")
example(rect)
dev.off()
unlink("myplot.jpeg")
}
}
\keyword{device}
