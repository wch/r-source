\name{Devices}
\title{List of Graphical Devices}
\usage{
postscript(\dots)
pictex(\dots)  
x11(\dots)
windows(\dots)
macintosh(\dots)
}
\alias{Devices}
\alias{device}
\description{The following graphics devices are currently available:
\itemize{
  \item \link{postscript} Writes PostScript graphics commands to a file
  \item \link{pictex} Writes LaTeX/PicTeX graphics commands to a file
  \item \link{x11} The graphics driver for the X11 Window system (Unix)
  \item \link{windows} The graphics driver for Windows
  \item \link{macintosh} The graphics driver for the Macintosh
}
}
\seealso{
  the individual help files for further information on any of the
  devices listed here;

  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}}.
}
\keyword{device}
