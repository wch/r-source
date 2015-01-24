% File src/library/grDevices/man/unix/quartz.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{quartz}
\alias{quartz}
\alias{quartz.options}
\alias{quartz.save}

\title{OS X Quartz Device}
\description{
  \code{quartz} starts a graphics device driver for the OS X System.
  It supports plotting both to the screen (the default) and to various
  graphics file formats.
}
\usage{
quartz(title, width, height, pointsize, family, antialias, type,
      file = NULL, bg, canvas, dpi)

quartz.options(\dots, reset = FALSE)

quartz.save(file, type = "png", device = dev.cur(), dpi = 100, ...)
}
\arguments{
  \item{title}{title for the Quartz window (applies to on-screen output
    only), default \code{"Quartz \%d"}.  A C-style format for an integer
    will be substituted by the device number (see the \code{file}
    argument to \code{\link{postscript}} for further details).}
  \item{width}{the width of the plotting area in inches.  Default \code{7}.}
  \item{height}{the height of the plotting area in inches.  Default \code{7}.}
  \item{pointsize}{the default pointsize to be used.  Default \code{12}.}
  \item{family}{this is the family name of the font
    that will be used by the device.  Default \code{"Arial"}.  This will
    be the base name of a font as  shown in Font Book.}
  \item{antialias}{whether to use antialiasing.  Default \code{TRUE}.}
  \item{type}{the type of output to use.  See \sQuote{Details} for
    more information.  Default \code{"native"}.}
  \item{file}{an optional target for the graphics device. The default,
    \code{NULL}, selects a default name where one is needed.  See
    \sQuote{Details} for more information.}
  \item{bg}{the initial background colour to use for the device.  Default
    \code{"transparent"}.  An opaque colour such as \code{"white"} will
    normally be required on off-screen types that support transparency
    such as \code{"png"} and \code{"tiff"}.}
  \item{canvas}{canvas colour to use for an on-screen device.  Default
    \code{"white"}, and will be forced to be an opaque colour.}
  \item{dpi}{resolution of the output.  The default (\code{NA_real_})
    for an on-screen display defaults to the resolution of
    the main screen, and to 72 dpi otherwise.  See \sQuote{Details}.}
  \item{\dots}{Any of the arguments to \code{quartz} except \code{file}.}
  \item{reset}{logical: should the defaults be reset to their defaults?}
  \item{device}{device number to copy from.}
}
\details{
  The defaults for all but one of the arguments of \code{quartz} are set
  by \code{quartz.options}: the \sQuote{Arguments} section gives the
  \sQuote{factory-fresh} defaults.

  The Quartz graphics device supports a variety of output types.
  On-screen output types are \code{""} or \code{"native"} or
  \code{"Cocoa"}.  Off-screen output types produce output files and
  utilize the \code{file} argument.  \code{type = "pdf"} gives PDF
  output.  The following bitmap formats may be supported (depending on
  the OS version): \code{"png"}, \code{"jpeg"}, \code{"jpg"},
  \code{"jpeg2000"}, \code{"tif"}, \code{"tiff"}, \code{"gif"},
  \code{"psd"} (Adobe Photoshop), \code{"bmp"} (Windows bitmap),
  \code{"sgi"} and \code{"pict"}.

  The \code{file} argument is used for off-screen drawing.  The actual
  file is only created when the device is closed (e.g., using
  \code{dev.off()}).  For the bitmap devices, the page number is
  substituted if a C integer format is included in the character string,
  e.g.\sspace{}\code{Rplot\%03d.png}.  (The result must be less than
  \code{PATH_MAX} characters long, and may be truncated if not. See
  \code{\link{postscript}} for further details.)  If a \code{file}
  argument is not supplied, the default is \code{Rplots.pdf} or
  \code{Rplot\%03d.\var{type}}.  Tilde expansion
  (see \code{\link{path.expand}}) is done.

  If a device-independent \R graphics font family is specified (e.g.,
  via \code{par(family =)} in the graphics package), the Quartz device
  makes use of the Quartz font database (see \code{quartzFonts}) to
  convert the R graphics font family to a Quartz-specific font family
  description.  The default conversions are (MonoType TrueType versions
  of) \code{Helvetica} for \code{sans}, \code{Times-Roman} for
  \code{serif} and \code{Courier} for \code{mono}.

  On-screen devices are launched with a semi-transparent canvas.  Once a
  new plot is created, the canvas is first painted with the
  \code{canvas} colour and then the current background colour (which can
  be transparent or semi-transparent).  Off-screen devices have no
  canvas colour, and so start with a transparent background where
  possible (e.g., \code{type = "png"} and \code{type = "tiff"}) --
  otherwise it appears that a solid white canvas is assumed in the
  Quartz code.  PNG and TIFF files are saved with a dark grey matte
  which will show up in some viewers, including \command{Preview}.

  \code{title} can be used for on-screen output.  It must be a single
  character string with an optional integer printf-style format that
  will be substituted by the device number.  It is also optionally used
  (without a format) to give a title to a PDF file.

  Calling \code{quartz()} sets \code{\link{.Device}} to \code{"quartz"}
  for on-screen devices and to \code{"quartz_off_screen"} otherwise.

  The font family chosen needs to cover the characters to be used:
  characters not in the font are rendered as empty oblongs.  For
  non-Western-European languages something other than the default of
  \code{"Arial"} is likely to be needed---one choice for Chinese is
  \code{"MingLiU"}.

  \code{quartz.save} is a modified version of \code{\link{dev.copy2pdf}}
  to copy the plot from the current screen device to a \code{quartz}
  device, by default to a PNG file.
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is Arial.
    \item Line widths are a multiple of 1/96 inch with no minimum set by \R.
    \item Circle radii are real-valued with no minimum set by \R.
    \item Colours are specified as sRGB.
  }
}

\note{
  For a long time the default font family was documented as
  \code{"Helvetica"} after it had been changed to \code{"Arial"} to work
  around a deficiency in OS X 10.4.   It may be changed back in future.
  
  A fairly common Mac problem is no text appearing on plots due to
  corrupted or duplicated fonts on your system.  You may be able to
  confirm this by using another font family,
  e.g.\sspace{}\code{family = "serif"}.
  Open the \code{Font Book} application (in \code{Applications}) and
  check the fonts that you are using.
}
\seealso{
  \code{\link{quartzFonts}}, \code{\link{Devices}}.

  \code{\link{png}} for way to access the bitmap types of this device
  via \R's standard bitmap devices.
}
\examples{\dontrun{
## put something like this is your .Rprofile to customize the defaults
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::quartz.options(width = 8, height = 6,
                                                pointsize = 10))
}}
\keyword{device}
