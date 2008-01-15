% File src/library/grDevices/man/unix/quartz.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{quartz}
\alias{quartz}
\alias{quartz.options}

\title{MacOS X Quartz device}
\description{
  \code{quartz} starts a graphics device driver for the Mac OS X System.
  It supports both on-screen and off-screen operations with the former
  being the default.
}
\usage{
quartz(title, width, height, pointsize, family, fontsmooth, antialias,
       type, file = NULL, bg, dpi)

quartz.options(\dots, reset = FALSE)
}
\arguments{
  \item{title}{title for the Quartz window (applies to on-screen output
    only). Default \code{"Quartz \%d"}.  A C-style format for an integer
    will be substituted by the device number (see the \code{file}
    argument to \code{\link{postscript}} for further details).}
  \item{width}{the width of the plotting area in inches.  Default \code{7}.}
  \item{height}{the height of the plotting area in inches.  Default \code{7}.}
  \item{pointsize}{the default pointsize to be used.  Default \code{12}.}
  \item{family}{this is the family name of the font
    that will be used by the device.  Default \code{"Helvetica"}.}
  \item{fontsmooth}{logical specifying if fonts should be
    smoothed. Default \code{TRUE}.}
  \item{antialias}{whether to use antialiasing. Default \code{TRUE}.}
  \item{type}{the type of output to use.  See \sQuote{Details} for
    more information.}
  \item{file}{an optional target for the graphics device.  See
    \sQuote{Details} for more information.}
  \item{bg}{background color to use for the device.  Default
    \code{"transparent"}.}
  \item{dpi}{resolution of the output.  The default (\code{NA_real_})
    for an on-screen display defaults to the resolution of
    the main screen, adn to 72 dpi otherwise.  See \sQuote{Details}.}
  \item{\dots}{Any of the arguments to \code{quartz} except \code{file}.}
  \item{reset}{logical: should the defaults be reset to their defaults?}
}
\details{
  The defaults for all but one of the arguments of \code{quartz} are set
  by \code{quartz.options}: the \sQuote{Arguments} section gives the
  \sQuote{factory-fresh} defaults.

  The Quartz graphics device supports a variety of output
  types.  On-screen output types are \code{"native"} (picks the best
  possible on-screen output), \code{"Cocoa"} (Mac OS X 10.4 and higher)
  and \code{"Carbon"} (Mac OS X 10.3 and earlier).   Off-screen output
  types produce output files and utilize the \code{file} argument. The
  following bitmap formats may be supported: \code{"png"},
  \code{"jpeg"}, \code{"tiff"}, \code{"gif"}, \code{"psd"} (Adobe
  Photoshop), \code{"bmp"} (Windows bitmap), \code{"sgi"} and
  \code{"pict"}.  The availability of some formats is
  OS-version-dependent.  To reproduce the default of older Quartz
  devices on-screen, set \code{dpi = 72} (for a permanent solution set
  \code{quartz.options(dpi = 72)}.

  The \code{file} argument is used for off-screen drawing. The actual
  file is only created when the device is closed (e.g. using
  \code{dev.off()}). Currently the output file is created on close
  and thus only the last page will be emitted. (This may change in the
  future.)

  If a device-independent \R graphics font family is specified
  (e.g., via \code{par(family=)} in the graphics package),
  the Quartz device makes use of the Quartz font database
  (see \code{quartzFonts}) to convert the R graphics font family
  to a Quartz-specific font family description.

  \code{title} can be used for on-screen output.  It must be a single
  character string with an optional integer printf-style format that
  will be substituted by the device number.

  Calling \code{quartz()} sets \code{\link{.Device}} to \code{"quartz"}.

  Line widths as controlled by \code{par(lwd=)} are in multiples of a
  point (1/72 inch) and values < 1 are silently converted to 1.
}
\seealso{
  \code{\link{quartzFonts}},
  \code{\link{Devices}}.
}
\keyword{device}
