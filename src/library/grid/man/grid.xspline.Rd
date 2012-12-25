% File src/library/grid/man/grid.xspline.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.xspline}
\alias{grid.xspline}
\alias{xsplineGrob}
\title{ Draw an Xspline }
\description{
  These functions create and draw an xspline, a curve drawn
  relative to control points.
}
\usage{
grid.xspline(...)
xsplineGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0),
            id = NULL, id.lengths = NULL,
            default.units = "npc",
            shape = 0, open = TRUE, arrow = NULL, repEnds = TRUE,
            name = NULL, gp = gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations of
    spline control points.}
  \item{y}{A numeric vector or unit object specifying y-locations of
    spline control points.}
  \item{id}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple xsplines.  All locations with the same
    \code{id} belong to the same xspline.}
  \item{id.lengths}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple xspline.  Specifies consecutive blocks of
    locations which make up separate xsplines.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y}
    are only given as numeric vectors.}
  \item{shape}{A numeric vector of values between -1 and 1, which
    control the shape of the spline relative to the control points.}
  \item{open}{A logical value indicating whether the spline is
    a line or a closed shape.}
  \item{arrow}{A list describing arrow heads to place at either end
    of the xspline, as produced by the \code{arrow} function.}
  \item{repEnds}{A logical value indicating whether the first and
    last control points should be replicated for drawing the curve
    (see Details below).}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments to be passed to \code{xsplineGrob}.}
}
\value{
  A grob object.
}
\details{
  Both functions create an xspline grob (a graphical object describing an
  xspline), but only \code{grid.xspline}
  draws the xspline.

  An xspline is a line drawn relative to control points.  For each
  control point, the line
  may pass through (interpolate) the control point or it
  may only approach (approximate) the control
  point;  the behaviour is determined by a shape parameter for each
  control point.

  If the shape parameter is greater than zero, the spline
  approximates the control points (and is
  very similar to a cubic B-spline when the shape is
  1).  If the shape parameter is less than zero, the spline interpolates
  the control points (and is very similar to a Catmull-Rom spline when
  the shape is -1).  If the shape parameter is 0, the spline forms a
  sharp corner at that control point.

  For open xsplines, the start and end control points must have a shape
  of 0 (and non-zero values are silently converted to zero without warning).

  For open xsplines, by default the start and end control points are
  actually replicated before the curve is drawn.  A curve is drawn
  between (interpolating or approximating)
  the second and third of each set of four control points, so
  this default behaviour ensures that
  the resulting curve starts at the first control point you have
  specified and ends at the last control point.
  The default
  behaviour can be turned off via the \code{repEnds} argument,
  in which case the curve that is drawn starts (approximately) at
  the second control point and ends (approximately) at the first and
  second-to-last control point.

  The \code{repEnds} argument is ignored for closed xsplines.

  Missing values are not allowed for \code{x} and \code{y}
  (i.e., it is not valid for a control point to be missing).

  For closed xsplines, a curve is automatically drawn
  between the final control point and the initial control point.
}
\references{
  Blanc, C. and Schlick, C. (1995),
  "X-splines : A Spline Model Designed for the End User",
  in \emph{Proceedings of SIGGRAPH 95}, pp. 377--386.
  \url{http://dept-info.labri.fr/~schlick/DOC/sig1.html}
}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{arrow}}.

  \code{\link{xspline}}.
}
\examples{
x <- c(0.25, 0.25, 0.75, 0.75)
y <- c(0.25, 0.75, 0.75, 0.25)

xsplineTest <- function(s, i, j, open) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  grid.points(x, y, default.units="npc", pch=16, size=unit(2, "mm"))
  grid.xspline(x, y, shape=s, open=open, gp=gpar(fill="grey"))
  grid.text(s, gp=gpar(col="grey"),
            x=unit(x, "npc") + unit(c(-1, -1, 1, 1), "mm"),
            y=unit(y, "npc") + unit(c(-1, 1, 1, -1), "mm"),
            hjust=c(1, 1, 0, 0),
            vjust=c(1, 0, 0, 1))
  popViewport()
}

pushViewport(viewport(width=.5, x=0, just="left",
                      layout=grid.layout(3, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.row=1))
grid.text("Open Splines", y=1, just="bottom")
popViewport()
xsplineTest(c(0, -1, -1, 0), 1, 1, TRUE)
xsplineTest(c(0, -1,  0, 0), 1, 2, TRUE)
xsplineTest(c(0, -1,  1, 0), 1, 3, TRUE)
xsplineTest(c(0,  0, -1, 0), 2, 1, TRUE)
xsplineTest(c(0,  0,  0, 0), 2, 2, TRUE)
xsplineTest(c(0,  0,  1, 0), 2, 3, TRUE)
xsplineTest(c(0,  1, -1, 0), 3, 1, TRUE)
xsplineTest(c(0,  1,  0, 0), 3, 2, TRUE)
xsplineTest(c(0,  1,  1, 0), 3, 3, TRUE)
popViewport()
pushViewport(viewport(width=.5, x=1, just="right",
                      layout=grid.layout(3, 3, respect=TRUE)))
pushViewport(viewport(layout.pos.row=1))
grid.text("Closed Splines", y=1, just="bottom")
popViewport()
xsplineTest(c(-1, -1, -1, -1), 1, 1, FALSE)
xsplineTest(c(-1, -1,  0, -1), 1, 2, FALSE)
xsplineTest(c(-1, -1,  1, -1), 1, 3, FALSE)
xsplineTest(c( 0,  0, -1,  0), 2, 1, FALSE)
xsplineTest(c( 0,  0,  0,  0), 2, 2, FALSE)
xsplineTest(c( 0,  0,  1,  0), 2, 3, FALSE)
xsplineTest(c( 1,  1, -1,  1), 3, 1, FALSE)
xsplineTest(c( 1,  1,  0,  1), 3, 2, FALSE)
xsplineTest(c( 1,  1,  1,  1), 3, 3, FALSE)
popViewport()
}
\keyword{dplot}
