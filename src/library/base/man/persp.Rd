\name{persp}
\title{Perspective Plots}
\usage{
persp(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), z,
        xlim = range(x), ylim = range(y), zlim = range(z, na.rm=T),
        theta = 0, phi = 15, d = 1, scale = TRUE, col, border, ...)
}
\alias{persp}
\arguments{
  \item{x,y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be in ascending order.  By default, equally
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively.}
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for
    convenience.}
  \item{xlim, ylim, zlim}{x-, y-  and z-limits.  The plot is produced
    so that the rectangular volume defined by these limits is visible.}
  \item{theta, phi}{angles defining the viewing direction.
    \code{theta} gives the azimuthal direction and \code{phi}
    the elevation.}
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
  \item{col}{the color of the surface facets.}
  \item{border}{color of the line drawn around the surface facets.}
  \item{\dots}{additional graphical parameters (see \code{\link{par}})
    and the arguments to \code{\link{title}} may also be supplied.}
}
\description{
  This function draws perspective plots of surfaces over the
  x-y plane.  The plots are produced by first transforming the
  coordinates to the interval [0,1].  The surface is then viewed
  by looking at the origin from a direction defined by \code{theta}
  and \code{phi}.  If \code{theta} and \code{phi} are both zero
  the viewing direction is directly down the negative y axis.
  Changing \code{theta} will vary the azimuth and changing \code{phi}
  the elevation.
}
\seealso{
  \code{\link{contour}} and \code{\link{image}}.
}
\examples{
# (1) The Obligatory Mathematical surface.
#     Rotated sinc function.

x <- seq(-10,10,length=50)
y <- x
f <- function(x,y)
{
        r <- sqrt(x^2+y^2)
        sin(r)/r
}
z <- outer(x,y,f)
z[is.na(z)] <- 1
persp(x, y, z, theta=30, phi=30, expand=0.5, col="lightblue")


# (2) Visualizing a simple DEM model

data(volcano)
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
persp(x, y, z, theta=120, phi=15, scale=FALSE)


# (3) Now something more complex
#     We border the surface, to make it more "slice like"
#     and color the top and sides of the surface differently.

zmin <- min(z)-20
z <- rbind(zmin, cbind(zmin, z, zmin), zmin)
x <- c(min(x)-1e-10, x, max(x)+1e-10)
y <- c(min(y)-1e-10, y, max(y)+1e-10)

fill <- matrix("green2", nr=nrow(z)-1, nc=ncol(z)-1)
fill[,1] <- "gray"
fill[,ncol(fill)] <- "gray"
fill[1,] <- "gray"
fill[nrow(fill),] <- "gray"

par(bg="lightblue")
persp(x, y, z, theta=120, phi=15, col=fill, scale=F)
title(main="Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.",
font.main=4)

}
\keyword{hplot}
\keyword{aplot}
