\name{x11}
\title{X Window System Graphics}
\usage{
X11(display = "", width = 7, height = 7, pointsize = 12,
    gamma = 1, colortype = getOption("X11colortype"),
    maxcubesize = 256)
}
\alias{x11}
\alias{X11}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \code{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
  \item{gamma}{the gamma correction factor.  This value is used
    to ensure that the colors displayed are linearly related to
    RGB values.  A value of around 0.5 is appropriate for many PC
    displays.  A value of 1.0 (no correction) is usually appropriate
    for high-end displays or Macintoshs.}
  \item{colortype}{the kind of color model to be used.  The
    possibilities are \code{"mono"}, \code{"gray"}, \code{"pseudo"},
    \code{"pseudo.cube"} and \code{"true"}.}
  \item{maxcubesize}{can be used to limited the size of color
    cube allocated for pseudocolor devices.}
}
\description{
  \code{X11} starts a graphics device driver for the X Window System
  (version 11).  This can only be done on machines that run X.
  \code{x11} is recognized as a synonym for \code{X11}.
}
\details{
  By default, an X11 device will use the best color rendering
  strategy that it can.  The choice can be overriden with the
  \code{colortype} parameter.  A value of \code{"mono"} results in
  black and white graphics, \code{"gray"} in grayscale and
  \code{"true"} in truecolor graphics (if this is possible).  The
  values \code{"pseudo"} and \code{"pseudo.cube"} provide color
  strategies for pseudocolor displays.  The first strategy provides
  on-demand color allocation which produces exact colors until
  the color resources of the display are exhausted.  The second
  causes a standard color cube to be set up, and requested colors
  are approximated by the closest value in the cube.  The default
  strategy for pseudocolor displays is \code{"pseudo"}.
}
\seealso{
  \code{\link{Devices}}.
}
\keyword{device}
