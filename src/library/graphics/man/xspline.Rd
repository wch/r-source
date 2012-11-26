% File src/library/graphics/man/xspline.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{xspline}
\alias{xspline}
\title{Draw an X-spline}
\description{
  Draw an X-spline, a curve drawn relative to control points.
}
\usage{
xspline(x, y = NULL, shape = 0, open = TRUE, repEnds = TRUE,
        draw = TRUE, border = par("fg"), col = NA, \dots)
}
\arguments{
  \item{x,y}{vectors containing the coordinates of the vertices
    of the polygon. See \code{\link{xy.coords}} for alternatives.}
  \item{shape}{A numeric vector of values between -1 and 1, which
    control the shape of the spline relative to the control points.}
  \item{open}{A logical value indicating whether the spline is
    an open or a closed shape.}
  \item{repEnds}{For open X-splines, a logical value indicating whether
    the first and last control points should be replicated for drawing
    the curve. Ignored for closed X-splines.}
  \item{draw}{logical: should the X-spline be drawn?  If false, a set of
    line segments to draw the curve is returned, and nothing is drawn.}
  \item{border}{the color to draw the curve.   Use \code{border = NA} to
    omit borders.}
  \item{col}{the color for filling the shape. The default,
    \code{NA}, is to leave unfilled.}
  \item{\dots}{\link{graphical parameters} such as \code{lty}, \code{xpd},
    \code{lend}, \code{ljoin} and \code{lmitre} can be given as arguments.}
}
\details{
  An X-spline is a line drawn relative to control points.  For each
  control point, the line may pass through (interpolate) the control
  point or it may only approach (approximate) the control point;  the
  behaviour is determined by a shape parameter for each control point.

  If the shape parameter is greater than zero, the spline approximates
  the control points (and is very similar to a cubic B-spline when the
  shape is 1).  If the shape parameter is less than zero, the spline
  interpolates the control points (and is very similar to a Catmull-Rom
  spline when the shape is -1).  If the shape parameter is 0, the spline
  forms a sharp corner at that control point.

  For open X-splines, the start and end control points must have a shape
  of 0 (and non-zero values are silently converted to zero).

  For open X-splines, by default the start and end control points are
  replicated before the curve is drawn.  A curve is drawn between
  (interpolating or approximating) the second and third of each set of
  four control points, so this default behaviour ensures that the
  resulting curve starts at the first control point you have specified
  and ends at the last control point.  The default behaviour can be
  turned off via the \code{repEnds} argument.
}
\value{
  If \code{draw = TRUE}, \code{NULL} otherwise a list with elements
  \code{x} and \code{y} which could be passed to \code{\link{lines}},
  \code{\link{polygon}} and so on.

  Invisible in both cases.
}
\note{
  Two-dimensional splines need to be created in an isotropic coordinate
  system.  Device coordinates are used (with an anisotropy correction if
  needed.)
}
\references{
  Blanc, C. and Schlick, C. (1995),
  \emph{X-splines : A Spline Model Designed for the End User},
  in \emph{Proceedings of SIGGRAPH 95}, pp. 377--386.
  \url{http://dept-info.labri.fr/~schlick/DOC/sig1.html}
}
\seealso{
  \code{\link{polygon}}.

  \code{\link{par}} for how to specify colors.
}
\examples{
## based on examples in ?grid.xspline

xsplineTest <- function(s, open = TRUE,
                        x = c(1,1,3,3)/4,
                        y = c(1,3,3,1)/4, ...) {
    plot(c(0,1), c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
    points(x, y, pch = 19)
    xspline(x, y, s, open, ...)
    text(x+0.05*c(-1,-1,1,1), y+0.05*c(-1,1,1,-1), s)
}
op <- par(mfrow = c(3,3), mar = rep(0,4), oma = c(0,0,2,0))
xsplineTest(c(0, -1, -1, 0))
xsplineTest(c(0, -1,  0, 0))
xsplineTest(c(0, -1,  1, 0))
xsplineTest(c(0,  0, -1, 0))
xsplineTest(c(0,  0,  0, 0))
xsplineTest(c(0,  0,  1, 0))
xsplineTest(c(0,  1, -1, 0))
xsplineTest(c(0,  1,  0, 0))
xsplineTest(c(0,  1,  1, 0))
title("Open X-splines", outer = TRUE)

par(mfrow = c(3,3), mar = rep(0,4), oma = c(0,0,2,0))
xsplineTest(c(0, -1, -1, 0), FALSE, col = "grey80")
xsplineTest(c(0, -1,  0, 0), FALSE, col = "grey80")
xsplineTest(c(0, -1,  1, 0), FALSE, col = "grey80")
xsplineTest(c(0,  0, -1, 0), FALSE, col = "grey80")
xsplineTest(c(0,  0,  0, 0), FALSE, col = "grey80")
xsplineTest(c(0,  0,  1, 0), FALSE, col = "grey80")
xsplineTest(c(0,  1, -1, 0), FALSE, col = "grey80")
xsplineTest(c(0,  1,  0, 0), FALSE, col = "grey80")
xsplineTest(c(0,  1,  1, 0), FALSE, col = "grey80")
title("Closed X-splines", outer = TRUE)

par(op)

x <- sort(stats::rnorm(5))
y <- sort(stats::rnorm(5))
plot(x, y, pch = 19)
res <- xspline(x, y, 1, draw = FALSE)
lines(res)
## the end points may be very close together,
## so use last few for direction
nr <- length(res$x)
arrows(res$x[1], res$y[1], res$x[4], res$y[4], code = 1, length = 0.1)
arrows(res$x[nr-3], res$y[nr-3], res$x[nr], res$y[nr], code = 2, length = 0.1)
}
\keyword{aplot}
