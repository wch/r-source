\name{persp}
\title{Perspective Plots}
\usage{
persp(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), z,
      xlim = range(x), ylim = range(y), zlim = range(z, na.rm = TRUE), 
      xlab = NULL, ylab = NULL, zlab = NULL,
      theta = 0, phi = 15, r = sqrt(3), d = 1,
      scale = TRUE, expand = 1, 
      col = NULL, border = NULL, ltheta = -135, lphi = 0,
      shade = NA, box = TRUE, axes = TRUE, nticks = 5, ticktype = "simple",
      ...) 
}
\alias{persp}
\arguments{
  \item{x, y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be in ascending order.  By default, equally
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively.}
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for
    convenience.}
  \item{xlim, ylim, zlim}{x-, y-  and z-limits.  The plot is produced
    so that the rectangular volume defined by these limits is visible.}
  \item{xlab, ylab, zlab}{titles for the axes. N.B. These must the
    character strings; expressions are not accepted.}
  \item{theta, phi}{angles defining the viewing direction.
    \code{theta} gives the azimuthal direction and \code{phi}
    the colatitude.}
  \item{r}{the distance of the eyepoint from the centre of the plotting box.}
  \item{d}{a value which can be used to vary the strength of
    the perspective transformation.  Values of \code{d} greater
    than 1 will lessen the perspective effect and values less
    and 1 will exaggerate it.}
  \item{scale}{before viewing the x, y and z coordinates of the
    points defining the surface are transformed to the interval
    [0,1].  If \code{scale} is \code{TRUE} the x, y and z coordinates
    are transformed separately.  If \code{scale} is \code{FALSE}
    the coordinates are scaled so that aspect ratios are retained.
    This is useful for rendering things like DEM information.}
  \item{expand}{a expansion factor applied to the \code{z}
    coordinates. Often used with \code{0 < expand < 1} to shrink the
    plotting box in the \code{z} direction.}
  \item{col}{the color of the surface facets.}
  \item{border}{the color of the line drawn around the surface facets.
    A value of \code{NA} will disable the drawing of borders.  This is
    sometimes useful when the surface is shaded.}
  \item{ltheta, lphi}{if finite values are specified for \code{ltheta}
    and \code{lphi}, the surface is shaded as though it was being
    illuminated from the direction specified by azimuth \code{ltheta}
    and colatitude \code{lphi}.}
  \item{shade}{the shade at a surface facet is computed as
    \code{((1+d)/2)^shade}, where \code{d} is the dot product of
    a unit vector normal to the facet and a unit vector in the
    direction of a light source.  Values of \code{shade} close
    to one yield shading similar to a point light source model
    and values close to zero produce no shading.  Values in the
    range 0.5 to 0.75 provide an approximation to daylight
    illumination.}
  \item{box}{should the bounding box for the surface be displayed.
    The default is \code{TRUE}.}
  \item{axes}{should ticks and labels be added to the box.  The 
    default is \code{TRUE}.  If \code{box} is \code{FALSE} then no
    ticks or labels are drawn.}
  \item{ticktype}{character: "simple" draws just an arrow parallel to 
    the axis to indicate direction of increase; "detailed" draws normal
    ticks as per 2D plots.}
  \item{nticks}{the (approximate) number of tick marks to draw on the
    axes.  Has no effect if \code{ticktype} is "simple".}
  \item{\dots}{additional graphical parameters (see \code{\link{par}})
    and the arguments to \code{\link{title}} may also be supplied.}
}
\description{
  This function draws perspective plots of surfaces over the
  x--y plane.
}
\details{
  The plots are produced by first transforming the
  coordinates to the interval [0,1].  The surface is then viewed
  by looking at the origin from a direction defined by \code{theta}
  and \code{phi}.  If \code{theta} and \code{phi} are both zero
  the viewing direction is directly down the negative y axis.
  Changing \code{theta} will vary the azimuth and changing \code{phi}
  the colatitude.
}
\seealso{
  \code{\link{contour}} and \code{\link{image}}.
}
\examples{
# (1) The Obligatory Mathematical surface.
#     Rotated sinc function.

x <- seq(-10, 10, length=50)
y <- x
f <- function(x,y)
{
    r <- sqrt(x^2+y^2)
    10 * sin(r)/r
}
z <- outer(x, y, f)
z[is.na(z)] <- 1
par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      xlab = "X", ylab = "Y", zlab = "Z") 
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z") 


# (2) Visualizing a simple DEM model

data(volcano)
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
persp(x, y, z, theta = 120, phi = 15, scale = FALSE, axes = FALSE)


# (3) Now something more complex
#     We border the surface, to make it more "slice like"
#     and color the top and sides of the surface differently.

zmin <- min(z) - 20
z <- rbind(zmin, cbind(zmin, z, zmin), zmin)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1)
fill[,1] <- "gray"
fill[,ncol(fill)] <- "gray"
fill[1,] <- "gray"
fill[nrow(fill),] <- "gray"

par(bg = "lightblue")
persp(x, y, z, theta = 120, phi = 15, col = fill, scale = FALSE, axes = FALSE)
title(main = "Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.",
      font.main = 4)

par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = fill, scale = FALSE,
      ltheta = -120, lphi = 15, shade = 0.65, axes = FALSE)
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)
}
\keyword{hplot}
\keyword{aplot}
