\name{Devices}
\title{List of Graphical Devices}
\usage{
#ifdef unix
x11(\dots)
#endif
#ifdef windows
windows(\dots)
#endif
#ifdef mac
macintosh(\dots)
#endif
postscript(\dots)
pdf(\dots)
pictex(\dots)  
#ifdef unix
png(\dots)
jpeg(\dots)
GTK(\dots)
GNOME(\dots)
#endif
#ifdef windows
win.metafile(\dots)
win.print(\dots)
png(\dots)
jpeg(\dots)
bmp(\dots)
#endif
xfig(\dots)
bitmap(\dots)

dev.interactive()
}
\alias{Devices}
\alias{device}
\alias{dev.interactive}
\description{The following graphics devices are currently available:
  \itemize{
    \item \code{\link{postscript}} Writes PostScript graphics commands to
    a file
    \item \code{\link{pdf}} Write PDF graphics commands to a file
    \item \code{\link{pictex}} Writes LaTeX/PicTeX graphics commands to a
    file
#ifdef windows
    \item \code{\link{windows}} The graphics driver for Windows (on screen,
    to printer and to Windows metafile).
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
    \item \code{\link{bmp}} BMP bitmap device
#endif
#ifdef mac
    \item \code{\link{macintosh}} The graphics driver for the Macintosh
#endif
    \item \code{\link{xfig}} Device for XFIG graphics file format
    \item \code{\link{bitmap}} bitmap pseudo-device via
    \code{GhostScript} (if available).
  }
#ifdef unix
  The following devices will be available if \R was compiled to use them
  and started with the appropriate \code{--gui} argument:
  \itemize{
    \item \code{\link{x11}} The graphics driver for the X11 Window
    system
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
    \item \code{\link{GTK}}, \code{\link{GNOME}} Graphics drivers for
    the GNOME GUI.
  }
  None of these are available under \code{\link[base:BATCH]{R CMD BATCH}}.
#endif
}
\value{
  \code{dev.interactive()} returns a logical, \code{TRUE} iff an
  interactive (screen) device is in use.
}
\seealso{
  The individual help files for further information on any of the
  devices listed here;

  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}},
  \code{\link{dev2bitmap}}.
#ifdef unix

  \code{\link{capabilities}} to see if \code{\link{x11}}, \code{\link{jpeg}}
  and \code{\link{png}} are available.
#endif
}
\keyword{device}
