% File src/library/grDevices/man/unix/quartz.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{quartz}
\alias{quartz}
\title{MacOS X Quartz device}
\description{
  \code{quartz} starts a graphics device driver for the Mac OS X System.
  It supports both on-screen and off-screen operations with the former
  being the default.
}
\usage{
quartz(title = "Quartz \%d", width = 5, height = 5, pointsize = 12,
       family = "Helvetica", fontsmooth = TRUE, antialias = TRUE,
       type = getOption("quartz.type"),
       file = NULL, bg = "transparent", dpi = getOption("quartz.dpi"))
}
\arguments{
  \item{title}{title for the Quartz window (applies to on-screen output only).}
  \item{width}{the width of the plotting area in inches.}
  \item{height}{the height of the plotting area in inches.}
  \item{pointsize}{the default pointsize to be used.}
  \item{family}{this is the family name of the Postscript font
    that will be used by the device.}
  \item{fontsmooth}{logical specifying if fonts should be smoothed.}
  \item{antialias}{whether to use antialiasing.}
  \item{type}{the type of output to use. See Details for more information. }
  \item{file}{an optional target for the graphics device. See Details
    for more information.}
  \item{bg}{background color to use for the device.}
  \item{dpi}{resolution of the output. For on-screen display \code{NULL}
    defaults to the resolution of the main screen, 72 dpi otherwise. See
    Details.}
}
\details{
  The Quartz graphics device supports a variety of output
  types.  On-screen output types are \code{"native"} (picks the best
  possible on-screen output), \code{"Cocoa"} (Mac OS X 10.4 and higher)
  and \code{"Carbon"} (Mac OS X 10.3 and earlier).  Off-screen output
  types produce output files and utilize the \code{file} argument. The
  following bitmap formats may be supported: \code{"png"},
  \code{"jpeg"}, \code{"tiff"}, \code{"gif"}, \code{"psd"} (Adobe
  Photoshop), \code{"bmp"} (Windows bitmap), \code{"sgi"} and
  \code{"pict"}. The availability of some formats is OS-dependent. In
  addition, special types of the form \code{"bitmap:<uti>"} can be
  used where \code{"<uti>"} specifies an Apple Core Graphics type.
   
  \emph{Device resolution}. The Quartz device can be used at any output
  resolution. The output resolution is specified by the \code{dpi}
  parameter. It can be either a numeric vector specifying \code{x} and
  \code{y} resolution in dots per inch (recycled as necessary) or
  \code{NULL} requesting the native resolution of the output (e.g. for
  screen output the resolution of the main screen is computed,
  otherwise a default of 72dpi is assumed). The Quartz device uses its
  own scaling in order to guarantee that output at different
  resoultions will be exactly identical when scaled to a common
  resolution. This means that a high-resolution off-screen output will
  look exactly the same when scaled down as the display output, no
  unpleasant surprises. It also handles devices with non-square pixels
  properly (i.e. a circle should always look like a circle and not an
  ellipse, even on 16:9 displays with 4:3 pixel layout).
   
  \emph{Important note:} previous Quartz graphics devices were ignoring
  the screen or output resolution by silenty setting it to 72 dpi. In
  order to replicate the behavior of older Quartz devices on-screen,
  set \code{dpi=72} (for permanent solution set
  \code{options(quartz.dpi=72)}.

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

  \code{title} can be used for on-screen output. It must be a single
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
