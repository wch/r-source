\name{Devices}
\title{List of Graphical Devices}
\usage{
postscript(\dots)
pictex(\dots)  
#ifdef unix
x11(\dots)
png(\dots)
jpeg(\dots)
#endif
#ifdef windows
windows(\dots)
png(\dots)
jpeg(\dots)
bmp(\dots)
#endif
#ifdef mac
macintosh(\dots)
#endif
bitmap(\dots)
}
\alias{Devices}
\alias{device}
\description{The following graphics devices are currently available:
\itemize{
  \item \code{\link{postscript}} Writes PostScript graphics commands to a file
  \item \code{\link{pictex}} Writes LaTeX/PicTeX graphics commands to a file
#ifdef unix
  \item \code{\link{x11}} The graphics driver for the X11 Window system (Unix)
  \item \code{\link{png}} PNG bitmap device
  \item \code{\link{jpeg}} JPEG bitmap device
#endif
#ifdef windows
  \item \code{\link{windows}} The graphics driver for Windows (on screen, to
    printer and to Windows metafile).
  \item \code{\link{png}} PNG bitmap device
  \item \code{\link{jpeg}} JPEG bitmap device
  \item \code{\link{bmp}} BMP bitmap device
#endif
#ifdef mac
  \item \code{\link{macintosh}} The graphics driver for the Macintosh
#endif
  \item \code{\link{xfig}} Device for XFIG graphics file format
  \item \code{\link{bitmap}} bitmap pseudo-device via \code{GhostScript} (if
    available).
}
}
\seealso{
  The individual help files for further information on any of the
  devices listed here;

  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}}, \code{\link{dev2bitmap}}
}
\keyword{device}
