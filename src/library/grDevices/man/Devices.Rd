% File src/library/grDevices/man/Devices.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{Devices}
\Rdversion{1.1}
\title{List of Graphical Devices}
\alias{Devices}
\alias{device}
\description{The following graphics devices are currently available:
  \itemize{
#ifdef windows
    \item \code{\link{windows}} The graphics device for Windows (on screen,
    to printer and to Windows metafile).
#endif
    \item \code{\link{postscript}} Writes PostScript graphics commands to
    a file
    \item \code{\link{pdf}} Write PDF graphics commands to a file
    \item \code{\link{pictex}} Writes LaTeX/PicTeX graphics commands to a
    file
#ifdef windows
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
    \item \code{\link{bmp}} BMP bitmap device
    \item \code{\link{tiff}} TIFF bitmap device
#endif
    \item \code{\link{xfig}} Device for XFIG graphics file format
    \item \code{\link{bitmap}} bitmap pseudo-device via
    \code{GhostScript} (if available).
  }
#ifdef unix
  The following devices will be functional if \R was compiled to use
  them (they exist but will return with a warning on other systems):
  \itemize{
    \item \code{\link{X11}} The graphics device for the X11 Window system
    \item \code{\link{bmp}} Windows bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{tiff}} TIFF bitmap device
    \item \code{\link{cairo_pdf}}, \code{cairo_ps} PDF and PostScript
    devices based on cairo graphics.

    \item \code{\link{quartz}} The graphics device for the Mac OS X
    native Quartz 2d graphics system.  (This is only functional on Mac
    OS X, but unlike \R < 2.7.0 can be used from the command line and
    not just from the GUI console.)
  }
#endif
}
\details{
  If no device is open, using a high-level graphics function will cause
  a device to be opened.  Which device is given by
  \code{\link{options}("device")} which is initially set as the most
  appropriate for each platform: a screen device for most interactive use and
  \code{\link{pdf}} (or the setting of \env{R_DEFAULT_DEVICE})
  otherwise.  The exception is interactive use under Unix if no screen
  device is known to be available, when \code{pdf()}is used.
}
\seealso{
  The individual help files for further information on any of the
  devices listed here;
#ifdef windows
  \code{\link{windows.options}},
#endif
#ifdef unix
  \code{\link{X11}.options}, \code{\link{quartz.options}},
#endif
  \code{\link{ps.options}} and \code{\link{pdf.options}} for how to
  customize devices.

  \code{\link{dev.interactive}},
  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}},
  \code{\link{dev2bitmap}}.
#ifdef unix

  \code{\link{capabilities}} to see if \code{\link{X11}}, \code{\link{jpeg}}
  \code{\link{png}} and \code{\link{quartz}} are available.
#endif
}
\examples{\dontrun{
## open the default screen device on this platform if no device is
## open
if(dev.cur() == 1) dev.new()
}}
\keyword{device}
