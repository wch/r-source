\name{Gnome}
\alias{GNOME}
\alias{gnome}
\title{GNOME Desktop Graphics Device}
\description{
  \code{gnome} starts a \acronym{GNOME} compatible device driver.
  \acronym{GNOME} is an acronym for \bold{G}NU \bold{N}etwork
  \bold{O}bject \bold{M}odel \bold{E}nvironment.
}
\usage{
gnome(display="", width=7, height=7, pointsize=12)
GNOME(display="", width=7, height=7, pointsize=12)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
}
\references{\url{http://www.gnome.org} and \url{http://www.gtk.org} for
  the \code{GTK+} (GIMP Tool Kit) libraries.
}
\author{Lyndon Drake \email{lyndon@stat.auckland.ac.nz}}
\note{
  This is still in development state.

  The GNOME device is only available when explicitly desired at
  configure/compile time, see the toplevel \file{INSTALL} file.
}
\seealso{\code{\link{x11}}, \code{\link{Devices}}.}
\examples{
\dontrun{
gnome(width=9)
}
}
\keyword{device}
