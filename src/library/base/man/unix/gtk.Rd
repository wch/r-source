\name{gtk}
\alias{gtk}
\alias{GTK}
\title{GTK+ Graphics Device}
\description{
  A variant of the X11 graphics device for use with the GNOME GUI.
}
\usage{
gtk(display="", width=7, height=7, pointsize=12)
GTK(display="", width=7, height=7, pointsize=12)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \code{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
}
\details{
  The GTK device is only available when running under UNIX with
  \code{--gui=gnome}.  Under that circumstance it is the default
  (auto-launched) device, but \code{\link{x11}} can also be used.

  The device has a toolbar in the GNOME style.
}
\author{Lyndon Drake \email{lyndon@stat.auckland.ac.nz}}
\references{
  \url{http://www.gtk.org} for the \code{GTK+} (GIMP Tool Kit) libraries.
}
\seealso{
  \code{\link{x11}},
  \code{\link{Devices}}.
}
\keyword{device}
