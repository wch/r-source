\name{gtk}
\alias{gtk}
\alias{GTK}
\title{GTK+ Graphics Device}
\description{
  This is a graphics device similar to the X11 device
  but using the Gtk widgets. This is now available via a separate
  package - gtkDevice - and can be used independently of the GNOME GUI
  for R. This package also allows a device to embedded within a
  Gtk-based GUI developed using the RGtk package.
  The gtkDevice package is available from CRAN.
}
\usage{
gtk(display="", width=7, height=7, pointsize=12)
GTK(display="", width=7, height=7, pointsize=12)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
}
\author{Original version by Lyndon Drake
  \email{lyndon@stat.auckland.ac.nz}.
  Reorganization by Martyn Plummer and Duncan Temple Lang.
}
\references{
  \url{http://www.gtk.org} for the \code{GTK+} (GIMP Tool Kit) libraries.
}
\seealso{
  \code{\link{x11}},
  \code{\link{Devices}}.
}
\keyword{device}
