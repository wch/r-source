\name{Devices}
\title{List of Graphical Devices}
\usage{
postscript(\dots)
pictex(\dots)  
#ifdef unix
x11(\dots)
#endif
#ifdef win
windows(\dots)
png(\dots)
jpeg(\dots)
bmp(\dots)
#endif
#ifdef mac
macintosh(\dots)
#endif
}
\alias{Devices}
\alias{device}
\description{The following graphics devices are currently available:
\itemize{
  \item \link{postscript} Writes PostScript graphics commands to a file
  \item \link{pictex} Writes LaTeX/PicTeX graphics commands to a file
#ifdef unix
  \item \link{x11} The graphics driver for the X11 Window system (Unix)
#endif
#ifdef win
  \item \link{windows} The graphics driver for Windows (on screen, to
    printer and to Windows metafile.
  \item \link{png} PNG bitmap device
  \item \link{jpeg} JPEG bitmap device
  \item \link{bmp} BMP bitmap device
#endif
#ifdef mac
  \item \link{macintosh} The graphics driver for the Macintosh
#endif
}
}
\seealso{
  the individual help files for further information on any of the
  devices listed here;

  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}}, \code{\link{dev2bitmap}}
}
\keyword{device}
