\name{x11}
\title{X Window System Graphics}
\usage{
X11(display = "", width = 7, height = 7, ps = 12)
x11(display = "", width = 7, height = 7, ps = 12)
}
\alias{x11}
\alias{X11}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \code{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{ps}{the default pointsize to be used.}
}
\description{
  \code{x11} starts a graphics device driver for the X Window System
  (version 11).  This can only be done on machines that run X.
}
\seealso{
  \code{\link{postscript}}, \code{\link{macintosh}},
  \code{\link{Devices}}.
}
\keyword{device}
