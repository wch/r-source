\name{png}
\alias{bmp}
\alias{png}
\alias{jpeg}
\title{BMP, JPEG and PNG graphics devices}
\description{
  A graphics device for BMP, JPEG or PNG format bitmap files.
}
\usage{
bmp(filename = "Rplot\%03d.bmp", width = 480, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
jpeg(filename = "Rplot\%03d.jpg", width = 480, height = 480, units = "px",
     pointsize = 12, quality = 75, bg = "white", res = NA,
     restoreConsole = TRUE)
png(filename = "Rplot\%03d.png", width = 480, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
}
\arguments{
  \item{filename}{the name of the output file, up to 511 characters.  The
    page number is substituted if a C integer format is included in the
    character string, as in the default.  (The result must be less than
    600 characters long.  See \code{\link{postscript}} for further
    details.)
  }
  \item{width}{the width of the device.}
  \item{height}{the height of the device.}
  \item{units}{The units in which \code{height} and \code{width} are
    given.  Can be \code{in} (inches), \code{px} (pixels), \code{cm} or
    \code{mm}.}
  \item{pointsize}{the default pointsize of plotted text, interpreted at
    72 dpi, so one point is approximately one pixel.}
  \item{bg}{the initial background colour: can be overridden by setting
    par("bg").}
  \item{quality}{the \sQuote{quality} of the JPEG image, as a
    percentage.  Smaller values will give more compression but also more
    degradation of the image.}
  \item{res}{The nominal resolution in dpi which will be recorded in the
    bitmap file, if a positive integer.  Also used for units other than
    the default.}
  \item{restoreConsole}{See the Details section of \code{\link{windows}}}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in modern web
  browsers. The PNG format is lossless and is best for line
  diagrams and blocks of solid colour. The JPEG format is lossy,
  but may be useful for image plots, for example. The BMP format is
  standard on Windows, and supported elsewhere.

  \code{png} supports transparent backgrounds on 16-bit
  (\sQuote{High Color}) or better screens: use \code{bg = "transparent"}.
  Not all PNG viewers render files with transparency correctly.

  Windows imposes limits on the size of bitmaps: these are not
  documented in the SDK and may depend on the version of Windows.
  It seems that \code{width} and \code{height} are each limited to
  \eqn{2^{15}-1}{2^15-1} and there is a 16Mb limit on the total amount
  of memory in Windows 95/98/ME.

  By default no resolution is recorded in the file.  Readers will often
  assume a nominal resolution of 72dpi when none is recorded.  As
  resolutions in PNG files are recorded in pixels/metre, the dpi value
  will be changed slightly.

  For graphics parameters that make use of dimensions in inches, a
  resolution of 96dpi is assumed.
  
  Both \code{bmp} and \code{png} will use a palette if there are less
  than 256 colours on the page, and record a 24-bit RGB file otherwise.
}
\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}
\section{Warnings}{
  Note that the \code{width} and \code{height} are in pixels not inches.
  A warning will be issued if both are less than 20.
  
  If you plot more than one page on one of these devices and do not
  include something like \code{\%d} for the sequence number in
  \code{file}, the file will contain the last page plotted.
}
\note{
  These devices effectively plot on a hidden screen and then copy the
  image to the required format.  This means that they have the same
  colour handling as the actual screen device, and work best if that is
  set to a 24-bit or 32-bit colour mode.
}

\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}

  \code{\link{bitmap}} provides an alternative way to generate PNG,
  JPEG  and other types of bitmap plots.  Devices \code{GDD} in
  CRAN package \pkg{GDD} and \code{CairoJPEG} / \code{CairoPNG} in CRAN
  package \pkg{Cairo} are further alternatives.
}
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
