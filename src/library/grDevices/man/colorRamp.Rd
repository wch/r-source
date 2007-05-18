\name{colorRamp}
\alias{colorRamp}
\alias{colorRampPalette}
\title{Color interpolation}
\description{
  These functions return functions that interpolate a set of given
  colors to create new color palettes (like \code{\link{topo.colors}}) and
  color ramps, functions that map the interval \eqn{[0, 1]} to colors
  (like \code{\link{grey}}).
}
\usage{
colorRamp(colors, bias = 1, space = c("rgb", "Lab"),
          interpolate = c("linear", "spline"))
colorRampPalette(colors, \dots)
}
\arguments{
  \item{colors}{Colors to interpolate }
  \item{bias}{A positive number. Higher values give more widely spaced
    colors at the high end.}
  \item{space}{Interpolation in RGB or CIE Lab color spaces}
  \item{interpolate}{Use spline or linear interpolation.}
  \item{\dots}{arguments to pass to \code{colorRamp}.}
}
\details{
  The CIE Lab color space is approximately perceptually uniform, and so
  gives smoother and more uniform color ramps. On the other hand,
  palettes that vary from one hue to another via white may have a more
  symmetrical appearance in RGB space.

  The conversion formulas in this function do not appear to be
  completely accurate and the color ramp may not reach the extreme
  values in Lab space.  Future changes in the \R color model may change
  the colors produced with \code{space="Lab"}.
}
\value{
  \code{colorRamp} returns a function that maps values between 0 and 1
  to colors.
  \code{colorRampPalette} returns a function that takes an integer
  argument and returns that number of colors interpolating the given
  sequence (similar to \code{\link{heat.colors}} or
  \code{\link{terrain.colors}}.
}

\seealso{
  Good starting points for interpolation are the "sequential"
  and "diverging" ColorBrewer palettes in the RColorBrewer package
}

\examples{
require(graphics)

## Here space="rgb" gives palettes that vary only in saturation,
## as intended.
## With space="Lab" the steps are more uniform, but the hues
## are slightly purple.
filled.contour(volcano,
               color.palette = colorRampPalette(c("red", "white", "blue")),
               asp = 1)
filled.contour(volcano,
               color.palette = colorRampPalette(c("red", "white", "blue"),
                                                space = "Lab"),
               asp = 1)

## Interpolating a 'sequential' ColorBrewer palette
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
filled.contour(volcano,
               color.palette = colorRampPalette(YlOrBr, space = "Lab"),
               asp = 1)
filled.contour(volcano,
               color.palette = colorRampPalette(YlOrBr, space = "Lab",
                                                bias = 0.5),
               asp = 1)

## 'jet.colors' is "as in Matlab" (and hurting the eye by over-saturation)
jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
filled.contour(volcano, color = jet.colors, asp = 1)

## space="Lab" helps when colors don't form a natural sequence
m <- outer(1:20,1:20,function(x,y) sin(sqrt(x*y)/3))
rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                space = "rgb")
Lab.palette <- colorRampPalette(c("red", "orange", "blue"),
                                space = "Lab")
filled.contour(m, col = rgb.palette(20))
filled.contour(m, col = Lab.palette(20))
}
\keyword{color}

