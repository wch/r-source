% File src/library/grDevices/man/Devices.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{Devices}
\title{List of Graphical Devices}
\alias{Devices}
\alias{device}
\description{The following graphics devices are currently available:
  \describe{
    \item{\code{\link{windows}}:}{On Windows only, the graphics device for
      Windows (on screen, to printer and to Windows metafile).}
    \item{\code{\link{pdf}}:}{Write PDF graphics commands to a file}
    \item{\code{\link{postscript}}:}{Writes PostScript graphics commands to
      a file}
    \item{\code{\link{xfig}}:}{Device for XFIG graphics file format}
    \item{\code{\link{bitmap}}:}{bitmap pseudo-device via
      \code{Ghostscript} (if available).}
    \item{\code{\link{pictex}}:}{Writes TeX/PicTeX graphics commands to a
      file (of historical interest only)
    }
  }
  The following devices will be functional if \R was compiled to use
  them (they exist but will return with a warning on other systems):
  \describe{
    \item{\code{\link{cairo_pdf}}, \code{cairo_ps}:}{PDF and PostScript
      devices based on cairo graphics.}
    \item{\code{\link{svg}}:}{SVG device based on cairo graphics}
    \item{\code{\link{png}}:}{PNG bitmap device}
    \item{\code{\link{jpeg}}:}{JPEG bitmap device}
    \item{\code{\link{bmp}}:}{BMP bitmap device}
    \item{\code{\link{tiff}}:}{TIFF bitmap device}
  }
  On Unix-alikes (incl. Mac) only:
  \describe{
    \item{\code{\link{X11}}:}{The graphics device for the X11 windowing system}
    \item{\code{\link{quartz}}:}{The graphics device for the macOS
    native Quartz 2d graphics system.  (This is only functional on
    macOS where it can be used from the \code{R.app} GUI and from the
    command line: but it will display on the local screen even for a
    remote session.)}
  }
}
\details{
  If no device is open, calling any high-level graphics function will cause
  a device to be opened.  Which device is determined by
  \code{\link{options}("device")} which is initially set as the most
  appropriate for each platform: a screen device for most interactive use and
  \code{\link{pdf}} (or the setting of \env{R_DEFAULT_DEVICE})
  otherwise.  The exception is interactive use under Unix if no screen
  device is known to be available, when \code{pdf()} is used.

  It is possible for an \R package (or an \R front-end such as RStudio)
  to provide further graphics devices
  and several packages on CRAN do so.  These include other devices outputting
  SVG and PGF/TiKZ (TeX-based graphics, see \url{http://pgf.sourceforge.net/}).
}
\seealso{
  The individual help files for further information on any of the
  devices listed here;
  \describe{
    \item{on Windows:}{\code{\link{windows.options}},}
    \item{on a Unix-alike:}{\code{\link{X11}.options}, \code{\link{quartz.options}},}
  }
  \code{\link{ps.options}} and \code{\link{pdf.options}} for how to
  customize devices.

  \code{\link{dev.interactive}},
  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}},
  \code{\link{dev2bitmap}}.

  On Unix-alikes only:\cr
  \code{\link{capabilities}} to see if \code{\link{X11}},
  \code{\link{jpeg}}, \code{\link{png}}, \code{\link{tiff}},
  \code{\link{quartz}} and the cairo-based devices are available.

}
\examples{\dontrun{
## open the default screen device on this platform if no device is
## open
if(dev.cur() == 1) dev.new()
}}
\keyword{device}
