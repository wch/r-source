\name{x11}
\alias{x11}
\alias{X11}
\title{X Window System Graphics}
\description{
  \code{X11} starts a graphics device driver for the X Window System
  (version 11).  This can only be done on machines that run X.
  \code{x11} is recognized as a synonym for \code{X11}.
}
\usage{
X11(display = "", width = 7, height = 7, pointsize = 12,
    gamma = 1, colortype = getOption("X11colortype"),
    maxcubesize = 256, bg = "transparent", canvas = "white",
    fonts = getOption("X11fonts"))
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.}
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
    \code{"pseudo.cube"} and \code{"true"}. Ignored if an \code{X11} is
    already open.}
  \item{maxcubesize}{can be used to limit the size of color
    cube allocated for pseudocolor devices.}
  \item{bg}{color.  The default background color.}
  \item{canvas}{color.  The color of the canvas, which is visible only
    when the background color is transparent.}
  \item{fonts}{X11 font description strings into which weight, slant and
    size will be substituted.  There are two, the first for fonts 1 to 4
    and the second for font 5, the symbol font.}
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

  \bold{Note:} All \code{X11} devices share a \code{colortype} which is
  set by the first device to be opened.  To change the \code{colortype}
  you need to close \emph{all} open \code{X11} devices then open one
  with the desired \code{colortype}.

  With \code{colortype} equal to \code{"pseudo.cube"} or \code{"gray"}
  successively smaller palettes are tried until one is completely
  allocated. If allocation of the smallest attempt fails the device will
  revert to \code{"mono"}.
}
\seealso{
  \code{\link{Devices}}.
}
\keyword{device}
