% File src/library/graphics/man/rasterImage.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{rasterImage}
\alias{rasterImage}
\title{Draw One or More Raster Images}
\usage{
rasterImage(image,
            xleft, ybottom, xright, ytop,
            angle = 0, interpolate = TRUE, \dots)
}
\arguments{
  \item{image}{a \code{raster} object, or an object that can be
    coerced to one by \code{\link{as.raster}}.}
  \item{xleft}{a vector (or scalar) of left x positions.}
  \item{ybottom}{a vector (or scalar) of bottom y positions.}
  \item{xright}{a vector (or scalar) of right x positions.}
  \item{ytop}{a vector (or scalar) of top y positions.}
  \item{angle}{angle of rotation (in degrees, anti-clockwise
    from positive x-axis, about the bottom-left corner).}
  \item{interpolate}{a logical vector (or scalar) indicating
    whether to apply linear interpolation to the image when drawing. }
  \item{\dots}{\link{graphical parameters}.}
}
\description{
  \code{rasterImage} draws a raster image at the given locations and sizes.
}
\details{
  The positions supplied, i.e., \code{xleft, \dots},
  are relative to the current plotting region.  If the x-axis goes from
  100 to 200 then \code{xleft} should be larger than 100 and \code{xright}
  should be less than 200.  The position vectors will be recycled to the
  length of the longest.

  Plotting raster images is not supported on all devices and may have
  limitations where supported, for example (e.g., for \code{postscript}
  and \code{X11(type = "Xlib")}) is restricted to opaque colors).
  Problems with the rendering of raster images have been reported by
  users of \code{windows()} devices under Remote Desktop, at least under
  its default settings.

  You should not expect a raster image to be re-sized when an on-screen
  device is re-sized: whether it is is device-dependent.
}
\seealso{
  \code{\link{rect}},
  \code{\link{polygon}}, and \code{\link{segments}} and others
  for flexible ways to draw shapes.

  \code{\link{dev.capabilities}} to see if it is supported.
}
\examples{
require(grDevices)
## set up the plot region:
op <- par(bg = "thistle")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
image <- as.raster(matrix(0:1, ncol = 5, nrow = 3))
rasterImage(image, 100, 300, 150, 350, interpolate = FALSE)
rasterImage(image, 100, 400, 150, 450)
rasterImage(image, 200, 300, 200 + xinch(.5), 300 + yinch(.3),
            interpolate = FALSE)
rasterImage(image, 200, 400, 250, 450, angle = 15, interpolate = FALSE)
par(op)
}
\keyword{aplot}
