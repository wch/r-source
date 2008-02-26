% File src/library/grDevices/man/unix/png.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{png}
\alias{png}
\alias{jpeg}
\alias{tiff}
\title{JPEG, PNG and TIFF graphics devices}
\description{
  Graphics devices for JPEG, PNG or TIFF format bitmap files.
}
\usage{
jpeg(filename = "Rplot\%03d.jpeg", width = 480, height = 480, units = "px",
     pointsize = 12, quality = 75, bg = "white", res = NA, \dots,
     type = c("Cairo", "Xlib"), antialias)

png(filename = "Rplot\%03d.png", width = 480, height = 480, units = "px",
    pointsize = 12, bg = "white",  res = NA, \dots,
    type = c("Cairo", "Xlib", "dCairo"), antialias)

tiff(filename = "Rplot%03d.tiff",width = 480, height = 480, units = "px",
     pointsize = 12,
     compression = c("none", "rle", "lzw", "jpeg", "zip"),
     bg = "white", res = NA,  \dots,
     type = c("Cairo", "Xlib"), antialias)
}
\arguments{
  \item{filename}{the name of the output file.
    The page number is substituted if a C integer format is included in
    the character string, as in the default.  (The result must be less
    than \code{PATH_MAX} characters long, and may be truncated if not.
    See \code{\link{postscript}} for further details.)  Tilde expansion
    is performed where supported by the platform.}
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
    bitmap file, if a positive integer.  Also used for \code{units}
    other than the default, and to convert points to pixels.}
  \item{\dots}{for \code{type = "Xlib"} only, additional arguments to
    the underlying \code{\link{X11}} device, such as \code{gamma} and
    \code{fonts}.}
  \item{type}{character string, one of \code{"Xlib"} (the only type
    prior to \R 2.7.0) or \code{"Cairo"} or \code{"dCairo"}.  The latter
    two will only be available if the system was compiled with support
    for Cairo -- otherwise \code{"Xlib"} will be used.  The default is
    set by \code{\link{X11.options}}.}
  \item{antialias}{for \code{type = "Cairo"}, giving the type of
    anti-aliasing (if any) to be used.  See \code{\link{X11}}.  The
    default is set by \code{\link{X11.options}}.}
}
\details{
  Plots in PNG and JPEG format can easily be converted to many other
  bitmap formats, and both can be displayed in modern web
  browsers.  The PNG format is lossless and is best for line
  diagrams and blocks of colour.  The JPEG format is lossy,
  but may be useful for image plots, for example.  TIFF is a meta-format:
  the default format written by \code{tiff} is lossless and stores RGB (and
  alpha where appropriate) values uncompressed---such files are widely
  accepted, which is their main virtue over PNG.

  \code{png} supports transparent backgrounds: use \code{bg =
    "transparent"}.  Not all PNG viewers render files with transparency
  correctly.  When transparency is in use  in the \code{type = "Xlib"}
  variant a very light grey is used as the background and so appear as
  transparent if used in the plot. This allows opaque white to be used,
  as in the example.  The \code{type = "Cairo"} variant allows
  semi-transparent colours, including on a transparent or
  semi-transparent background.

  \code{tiff(type = "Cairo")} supports semi-transparent colours,
  including on a transparent or semi-transparent background.

  \R can be compiled without support for each of these devices: this
  will be reported if you attempt to use them on a system where they are
  not supported.  For \code{type = "Xlib"} they may not be usable unless
  the X11 display is available to the owner of the \R process.  With
  cairo version 1.0, \code{type = "Cairo"} is unavailable (but
  \code{type="bCairo"} works).

  By default no resolution is recorded in the file.  Viewers will often
  assume a nominal resolution of 72dpi when none is recorded.  As
  resolutions in PNG files are recorded in pixels/metre, the reported
  dpi value will be changed slightly.

  For graphics parameters that make use of dimensions in inches
  (including font sizes in points) the resolution used is \code{res} (or
  72dpi if unset).

  \code{png} will use a palette if there are less than 256 colours on
  the page, and record a 24-bit RGB file otherwise (or a 32-bit RGBA
  file if \code{type = "Cairo"} and non-opaque colours are used).

  \code{png(type = "dCairo"} uses cairo directly to output the file --
  the resolution is not recorded and the PNG format used is chosen by
  cairo (and undocumented -- it is currently 32-bit RGBA, that is with
  an alpha channel whether used or not).
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}

\section{Warnings}{
  Note that by default the \code{width} and \code{height} are in pixels
  not inches.  A warning will be issued if both are less than 20.
  
  If you plot more than one page on one of these devices and do not
  include something like \code{\%d} for the sequence number in
  \code{file}, the file will contain the last page plotted.
}

\note{
  For \code{type = "Xlib"} these devices are based on the \code{\link{X11}}
  device.  The colour model used will be that set up by
  \code{X11.options} at the time the first \code{X11},
  \code{jpeg} or \code{png} device was opened (or the first after all
  such devices have been closed).
}

\author{Guido Masarotto and Brian Ripley}

\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}

  \code{\link{capabilities}} to see if these devices are
  supported by this build of \R, and if \code{type = "Cairo"} is supported.

  \code{\link{bitmap}} provides an alternative way to generate PNG and
  JPEG plots that does not depend on accessing the X11 display but does
  depend on having GhostScript installed.  (Devices \code{GDD} in
  CRAN package \pkg{GDD} and \code{CairoJPEG} / \code{CairoPNG} in CRAN
  package \pkg{Cairo} are further alternatives using several other
  additional pieces of software.)
}

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is in pixels.
    \item Font sizes are in big points interpreted at \code{res} dpi.
    \item The default font family is Helvetica.
    \item Line widths in 1/96 inch, minimum one pixel for \code{type =
      "Xlib"}.
    \item For \code{type = "Xlib"} circle radii are in pixels with
    minimum one.
    \item Colours are interpreted by the viewing application.
  }
}

\examples{
## these examples will work only if the devices are available
## and either cairo or an X11 display is available.

## copy current plot to a (large) PNG file
\dontrun{dev.print(png, file="myplot.png", width=1024, height=768)}
\donttest{
png(file="myplot.png", bg="transparent")
plot(1:10)
rect(1, 5, 3, 7, col="white")
dev.off()

## will make myplot1.jpeg and myplot2.jpeg
jpeg(file="myplot\%d.jpeg")
example(rect)
dev.off()
}}
\keyword{device}
