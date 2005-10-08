\name{png}
\alias{png}
\alias{jpeg}
\title{JPEG and PNG graphics devices}
\description{
  A graphics device for JPEG or PNG format bitmap files.
}
\usage{
jpeg(filename = "Rplot\%03d.jpeg", width = 480, height = 480,
     pointsize = 12, quality = 75, bg = "white", res = NA, \dots)

png(filename = "Rplot\%03d.png", width = 480, height = 480,
    pointsize = 12, bg = "white",  res = NA,\dots)
}
\synopsis{
jpeg(filename = "Rplot\%03d.jpeg", width = 480, height = 480,
     pointsize = 12, quality = 75, gamma = 1,
     colortype = getOption("X11colortype"), maxcubesize = 256,
     bg = "white", fonts = getOption("X11fonts"), res = NA)

png(filename ="Rplot\%03d.png", width = 480, height = 480,
    pointsize = 12, gamma = 1, colortype = getOption("X11colortype"),
    maxcubesize = 256, bg = "white", fonts = getOption("X11fonts"),
    res = NA)
}
\arguments{
  \item{filename}{the name of the output file.
    The page number is substituted if a C integer format is included in
    the character string, as in the default.  (The result must be less
    than \code{PATH_MAX} characters long, and may be truncated if not.
    See \code{\link{postscript}} for further details.)  Tilde expansion
    is performed where supported by the platform.}
  \item{width}{the width of the device in pixels.}
  \item{height}{the height of the device in pixels.}
  \item{pointsize}{the default pointsize of plotted text.}
  \item{quality}{the \sQuote{quality} of the JPEG image, as a
    percentage.  Smaller values will give more compression but also more
    degradation of the image.}
  \item{bg}{default background colour.}
 \item{res}{The nominal resolution in dpi which will be recorded in the
   bitmap file, if a positive integer.}
  \item{\dots}{additional arguments to the \code{\link{X11}} device.}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in modern web
  browsers.  The PNG format is lossless and is best for line
  diagrams and blocks of solid colour.  The JPEG format is lossy,
  but may be useful for image plots, for example.

  \code{png} supports transparent backgrounds: use \code{bg = "transparent"}.
  Not all PNG viewers render files with transparency correctly.
  When transparency is in use a very light grey is used as the
  background and so will appear as transparent if used in the plot.
  This allows opaque white to be used, as on the example.

  \R can be compiled without support for either or both of these
  devices: this will be reported if you attempt to use them on a system
  where they are not supported.  They may not be usable unless the X11
  display is available to the owner of the \R process.

  By default no resolution is recorded in the file.  Readers will often
  assume nominal resolution of 72dpi when none is recorded.  As
  resolutions in PNG files are recorded in pixels/metre, the dpi value
  will be changed slightly.
}
\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}
\section{Warning}{
  If you plot more than one page on one of these devices and do not
  include something like \code{\%d} for the sequence number in
  \code{file}, the file will contain the last page plotted.
}
\note{
  These are based on the \code{\link{X11}} device, so the additional
  arguments to that device work, but are rarely appropriate.  The
  colour handling will be that of the \code{X11} device in use.
}
\author{Guido Masarotto and Brian Ripley}

\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}

  \code{\link{capabilities}} to see if these devices are
  supported by this build of \R.

  \code{\link{bitmap}} provides an alternative way to generate PNG and
  JPEG plots that does not depend on accessing the X11 display but does
  depend on having GhostScript installed.
}
\examples{
## these examples will work only if the devices are available
## and the X11 display is available.

## copy current plot to a PNG file
\dontrun{dev.print(png, file="myplot.png", width=480, height=480)

png(file="myplot.png", bg="transparent")
plot(1:10)
rect(1, 5, 3, 7, col="white")
dev.off()

jpeg(file="myplot.jpeg")
example(rect)
dev.off()
}}
\keyword{device}
