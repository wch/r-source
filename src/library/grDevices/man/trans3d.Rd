% File src/library/grDevices/man/trans3d.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{trans3d}
\alias{trans3d}
\title{3D to 2D Transformation for Perspective Plots}
\description{
  Projection of 3-dimensional to 2-dimensional points using a 4x4
  viewing transformation matrix.  Mainly for adding to
  perspective plots such as \code{\link{persp}}.
}
\usage{
trans3d(x, y, z, pmat, continuous = FALSE, verbose = TRUE)
}
\arguments{
  \item{x, y, z}{numeric vectors of equal length, specifying points in
    3D space.}
  \item{pmat}{a \eqn{4 \times 4}{4 x 4} \emph{viewing transformation matrix},
    suitable for projecting the 3D coordinates \eqn{(x,y,z)} into the 2D
    plane using homogeneous 4D coordinates \eqn{(x,y,z,t)};
    such matrices are returned by \code{\link{persp}()}.}
  \item{continuous}{logical flag specifying if the transformation should
    check if the transformed points are continuous in the sense that they
    do not jump over \eqn{a/0} discontinuity.  As these assume
    \code{(x,y,z)} to describe a continuous curve, the default must be
    false.  In case of projecting such a curve however, setting
    \code{continuous=TRUE} may be advisable.}
  \item{verbose}{only for \code{continuous=TRUE}, indicates if a warning
    should be issued when points are cut off.}
}
\value{
  a list with two components
  \item{x,y}{the projected 2d coordinates of the 3d input \code{(x,y,z)}.}
}
\seealso{ \code{\link{persp}} }
\examples{
## See  help(persp) {after attaching the 'graphics' package}
##      -----------

## Example for 'continuous = TRUE' (vs default):
require(graphics)
x <- -10:10/10 # [-1, 1]
y <- -16:16/16 # [-1, 1] ==> z = fxy := outer(x,y) is also in [-1,1]

p <- persp(x, y, fxy <- outer(x,y), phi = 20, theta = 15, r = 3, ltheta = -75,
           shade = 0.8, col = "green3", ticktype = "detailed")
## 5 axis-parallel auxiliary lines in x-y  and y-z planes :
lines(trans3d(-.5 , y=-1:1, z=min(fxy),  pmat=p), lty=2)
lines(trans3d(  0 , y=-1:1, z=min(fxy),  pmat=p), lty=2)
lines(trans3d(-1:1, y= -.7, z=min(fxy),  pmat=p), lty=2)
lines(trans3d( -1,  y= -.7, z=c(-1,1) ,  pmat=p), lty=2)
lines(trans3d( -1,  y=-1:1, z= -.5    ,  pmat=p), lty=2)
## 2 pillars to carry the horizontals below:
lines(trans3d(-.5 , y= -.7, z=c(-1,-.5), pmat=p), lwd=1.5, col="gray10")
lines(trans3d( 0  , y= -.7, z=c(-1,-.5), pmat=p), lwd=1.5, col="gray10")
## now some "horizontal rays" (going from center to very left or very right):
doHor <- function(x1, x2, z, CNT=FALSE, ...)
    lines(trans3d(x=seq(x1, x2, by=0.5), y= -0.7, z = z, pmat = p, continuous = CNT),
          lwd = 3, type="b", xpd=NA, ...)
doHor(-10,  0, z = -0.5, col = 2)  # x in [-10, 0] -- to the very left : fine
doHor(-.5,  2, z = -0.52,col = 4) # x in [-0.5, 2] only {to the right} --> all fine
## but now, x in [-0.5, 20] -- "too far" ==> "wrap around" problem (without 'continuous=TRUE'):
doHor(-.5, 20, z = -0.58, col = "steelblue", lty=2)
## but it is fixed with continuous = CNT = TRUE:
doHor(-.5, 20, z = -0.55, CNT=TRUE, col = "skyblue")
}
\keyword{dplot}
