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
\alias{windows}
 % ^^^^^ 'windows' does not yet have its own doc page !!
%   -----------------
%  |||||  'VALUES' below should be replaced by 'DESCRIPTION' +  'ITEMIZE'!
%  vvvvv
\value{The following graphics devices are currently available:
  \item{\link{postscript}}{Writes PostScript graphics commands into
    file)}
  \item{\link{pictex}}{Writes LaTeX/PicTeX graphics commands into file}
  \item{\link{x11}}{The graphics driver for the  X11 Window system
    (Unix)}
  \item{\link{windows}}{The graphics driver for MS Windows}  
  \item{\link{macintosh}}{The graphics driver for the Macintosh}
}
\seealso{
  the individual help files for further information on any of the
  devices listed here;

  \code{\link{print.plot}}, \code{\link{save.plot}},
  \code{\link{graphics.off}}, \code{\link{image}}.
}
\keyword{device}
