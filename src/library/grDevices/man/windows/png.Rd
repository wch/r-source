% File src/library/grDevices/man/windows/png.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{png}
\alias{png}
\alias{jpeg}
\alias{tiff}
\alias{bmp}
\title{BMP, JPEG, PNG and TIFF graphics devices}
\description{
  Graphics devices for BMP, JPEG, PNG and TIFF format bitmap files.
}
\usage{
bmp(filename = "Rplot\%03d.bmp", width = 480, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA,
    restoreConsole = TRUE)
jpeg(filename = "Rplot\%03d.jpg", width = 480, height = 480,
     units = "px", pointsize = 12, quality = 75, bg = "white",
     res = NA, restoreConsole = TRUE)
png(filename = "Rplot\%03d.png", width = 480, height = 480,
    units = "px", pointsize = 12, bg = "white", res = NA,
    restoreConsole = TRUE)
tiff(filename = "Rplot\%03d.tif", width = 480, height = 480,
     units = "px", pointsize = 12,
     compression = c("none", "rle", "lzw", "jpeg", "zip"),
     bg = "white", res = NA,
     restoreConsole = TRUE)
}
\arguments{
  \item{filename}{the name of the output file, up to 511 characters.  The
    page number is substituted if a C integer format is included in the
    character string, as in the default, and tilde-expansion is
    performed (see \code{\link{path.expand}}).   (The result must be less than
    600 characters long.  See \code{\link{postscript}} for further
    details.)
  }
  \item{width}{the width of the device.}
  \item{height}{the height of the device.}
  \item{units}{The units in which \code{height} and \code{width} are
    given.  Can be \code{px} (pixels, the default), \code{in} (inches),
    \code{cm} or \code{mm}.}
  \item{pointsize}{the default pointsize of plotted text, interpreted as
    big points (1/72 inch) at \code{res} dpi.}
  \item{bg}{the initial background colour: can be overridden by setting
    par("bg").}
  \item{quality}{the \sQuote{quality} of the JPEG image, as a
    percentage.  Smaller values will give more compression but also more
    degradation of the image.}
  \item{compression}{the type of compression to be used.}
  \item{res}{The nominal resolution in dpi which will be recorded in the
    bitmap file, if a positive integer.  Also used for units other than
    the default.  If not specified, taken as 72 dpi to set the size of
    text and line widths.}
  \item{restoreConsole}{See the \sQuote{Details} section of
    \code{\link{windows}}.}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in modern web
  browsers.  The PNG format is lossless and is best for line
  diagrams and blocks of colour.  The JPEG format is lossy,
  but may be useful for image plots, for example.  The BMP format is
  standard on Windows, and supported by most viewers elsewhere.
  TIFF is a meta-format: the default format written by \code{tiff} is lossless
  and stores RGB values uncompressed---such files are widely accepted,
  which is their main virtue over PNG.

  Windows imposes limits on the size of bitmaps: these are not
  documented in the SDK and may depend on the version of Windows.
  It seems that \code{width} and \code{height} are each limited to
  \eqn{2^{15}-1}{2^15-1}.  In addition, there are limits on the total
  number of pixels which depend on the graphics hardware.

  By default no resolution is recorded in the file.  Viewers will often
  assume a nominal resolution of 72dpi when none is recorded.  As
  resolutions in PNG files are recorded in pixels/metre, the reported
  dpi value will be changed slightly.

  For graphics parameters that make use of dimensions in inches,
  \code{res} dpi (default 72) is assumed.
  
  Both \code{bmp} and \code{png} will use a palette if there are less
  than 256 colours on the page, and record a 24-bit RGB file otherwise.

  \code{png} supports transparent backgrounds on 16-bit (\sQuote{High
    Color}) or better screens: use \code{bg = "transparent"}.  There is
  also support for semi-transparent colours of lines, fills and text.
  However, as there is only partial support for transparency in the
  graphics toolkit used, if there is a transparent background
  semi-transparent colours are painted onto a slightly off-white
  background and hence the pixels are opaque.

  Not all PNG viewers render files with transparency correctly.
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}

\section{Warnings}{
  Note that by default the \code{width} and \code{height} values are in
  pixels not inches.  A warning will be issued if both are less than 20.
  
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

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is in pixels.
    \item Font sizes are in big points interpreted at \code{res} dpi.
    \item The default font family is Arial.
    \item Line widths are a multiple of 1/96 inch (interpreted at
    \code{res} dpi), with a minimum of one pixel. 
    \item The minimum radius of a circle is 1 pixel.
    \item Colours are interpreted by the viewing application.
  }
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
