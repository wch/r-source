% File src/library/graphics/man/persp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{persp}
\alias{persp}
\alias{persp.default}
\title{Perspective Plots}
\description{
  This function draws perspective plots of a surface over the
  x--y plane.  \code{persp} is a generic function.
}
\usage{
persp(x, \dots)

\method{persp}{default}(x = seq(0, 1, length.out = nrow(z)),
      y = seq(0, 1, length.out = ncol(z)),
      z, xlim = range(x), ylim = range(y),
      zlim = range(z, na.rm = TRUE),
      xlab = NULL, ylab = NULL, zlab = NULL,
      main = NULL, sub = NULL,
      theta = 0, phi = 15, r = sqrt(3), d = 1,
      scale = TRUE, expand = 1,
      col = "white", border = NULL, ltheta = -135, lphi = 0,
      shade = NA, box = TRUE, axes = TRUE, nticks = 5,
      ticktype = "simple", \dots)
}
\arguments{
  \item{x, y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be in ascending order.  By default, equally
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively.}
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for
    convenience.}
  \item{xlim, ylim, zlim}{x-, y-  and z-limits.  These should be chosen
    to cover the range of values of the surface: see \sQuote{Details}.}
  \item{xlab, ylab, zlab}{titles for the axes.  N.B. These must be
    character strings; expressions are not accepted.  Numbers will be
    coerced to character strings.}
  \item{main, sub}{main and sub title, as for \code{\link{title}}.}
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
  \item{col}{the color(s) of the surface facets.  Transparent colours are
    ignored.  This is recycled to the \eqn{(nx-1)(ny-1)} facets.}
  \item{border}{the color of the line drawn around the surface facets.
    The default, \code{NULL}, corresponds to \code{par("fg")}.
    A value of \code{NA} will disable the drawing of borders: this is
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
  \item{ticktype}{character: \code{"simple"} draws just an arrow
    parallel to the axis to indicate direction of increase;
    \code{"detailed"} draws normal ticks as per 2D plots.}
  \item{nticks}{the (approximate) number of tick marks to draw on the
    axes.  Has no effect if \code{ticktype} is \code{"simple"}.}
  \item{\dots}{additional \link{graphical parameters} (see \code{\link{par}}).}
}
\value{
  \code{persp()} returns the \emph{viewing transformation matrix}, say
  \code{VT}, a \eqn{4 \times 4}{4 x 4} matrix suitable for projecting 3D
  coordinates \eqn{(x,y,z)} into the 2D plane using homogeneous 4D
  coordinates \eqn{(x,y,z,t)}.  It can be used to superimpose
  additional graphical elements on the 3D plot, by
  \code{\link{lines}()} or \code{\link{points}()}, using the
  function \code{\link{trans3d}()}.
}
\details{
  The plots are produced by first transforming the (x,y,z)
  coordinates to the interval [0,1] using the limits supplied or
  computed from the range of the data.  The surface is then viewed
  by looking at the origin from a direction defined by \code{theta}
  and \code{phi}.  If \code{theta} and \code{phi} are both zero
  the viewing direction is directly down the negative y axis.
  Changing \code{theta} will vary the azimuth and changing \code{phi}
  the colatitude.

  There is a hook called \code{"persp"} (see \code{\link{setHook}})
  called after the plot is completed, which is used in the
  testing code to annotate the plot page.  The hook function(s) are
  called with no argument.

  Notice that \code{persp} interprets the \code{z} matrix as a table of
  \code{f(x[i], y[j])} values, so that the x axis corresponds to row
  number and the y axis to column number, with column 1 at the bottom,
  so that with the standard rotation angles, the top left corner of the
  matrix is displayed at the left hand side, closest to the user.

  The sizes and fonts of the axis labels and the annotations for
  \code{ticktype = "detailed"} are controlled by graphics parameters
  \code{"cex.lab"}/\code{"font.lab"} and
  \code{"cex.axis"}/\code{"font.axis"} respectively.

  The bounding box is drawn with edges of faces facing away from the
  viewer (and hence at the back of the box) with solid lines and other
  edges dashed and on top of the surface.  This (and the plotting of the
  axes) assumes that the axis limits are chosen so that the surface
  is within the box, and the function will warn if this is not the case.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{contour}} and \code{\link{image}}; \code{\link{trans3d}}.

  Rotatable 3D plots can be produced by package \CRANpkg{rgl}: other
  ways to produce static perspective plots are available in packages
  \CRANpkg{lattice} and \CRANpkg{scatterplot3d}.
}
\examples{
require(grDevices) # for trans3d
## More examples in  demo(persp) !!
##                   -----------

# (1) The Obligatory Mathematical surface.
#     Rotated sinc function.

x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )"
) -> res
round(res, 3)

# (2) Add to existing persp plot - using trans3d() :

xE <- c(-10,10); xy <- expand.grid(xE, xE)
points(trans3d(xy[,1], xy[,2], 6, pmat = res), col = 2, pch = 16)
lines (trans3d(x, y = 10, z = 6 + sin(x), pmat = res), col = 3)

phi <- seq(0, 2*pi, len = 201)
r1 <- 7.725 # radius of 2nd maximum
xr <- r1 * cos(phi)
yr <- r1 * sin(phi)
lines(trans3d(xr,yr, f(xr,yr), res), col = "pink", lwd = 2)
## (no hidden lines)

# (3) Visualizing a simple DEM model

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
## Don't draw the grid lines :  border = NA
par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)

# (4) Surface colours corresponding to z-values

par(bg = "white")
x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)

par(op)
}
\keyword{hplot}
\keyword{aplot}
