\name{x11}
\title{X Window System Graphics}
\usage{
X11(display = "", width = 7, height = 7, ps = 12, gamma = 1)
x11(display = "", width = 7, height = 7, ps = 12, gamma = 1)
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
  \item{gamma}{the gamma correction factor.  This value is used
    to ensure that the colors displayed are linearly related to
    RGB values.  A value of 0.5 is appropriate for many PC displays.
    A value of 1.0 (no correction) is usually appropriate for
    high-end displays.}
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
