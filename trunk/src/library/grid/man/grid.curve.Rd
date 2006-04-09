\name{grid.curve}
\alias{grid.curve}
\alias{curveGrob}
\alias{arcCurvature}
\title{ Draw a Curve Between Locations }
\description{
  These functions create and draw a curve from one location
  to another.
}
\usage{
grid.curve(...)
curveGrob(x1, y1, x2, y2, default.units = "npc",
          curvature = 1, angle = 90, ncp = 1, shape = 0.5,
          square = TRUE, squareShape = 1,
          inflect = FALSE, arrow = NULL, debug = FALSE,
          name = NULL, gp = gpar(), vp = NULL)
arcCurvature(theta)
}
\arguments{
  \item{x1}{A numeric vector or unit object specifying the x-location of
    the start point.}
  \item{y1}{A numeric vector or unit object specifying the y-location of
    the start point.}
  \item{x2}{A numeric vector or unit object specifying the x-location of
    the end point.}
  \item{y2}{A numeric vector or unit object specifying the y-location of
    the end point.}
  \item{default.units}{A string indicating the default units to use
    if \code{x1}, \code{y1}, \code{x2} or \code{y2}
    are only given as numeric values.}
  \item{curvature}{A numeric value giving the amount of curvature.
    Negative values produce left-hand curves, positive values
    produce right-hand curves, and zero produces a straight line.}
  \item{angle}{A numeric value between 0 and 180,
    giving an amount to skew the control
    points of the curve.  Values less than 90 skew the curve towards
    the start point and values greater than 90 skew the curve
    towards the end point.}
  \item{ncp}{The number of control points used to draw the curve.
    More control points creates a smoother curve.}
  \item{shape}{A numeric vector of values between -1 and 1, which
    control the shape of the curve relative to its control points.
    See \code{grid.xspline} for more details.}
  \item{square}{A logical value that controls whether control
    points for the curve are created city-block fashion or
    obliquely.  When \code{ncp} is 1 and \code{angle} is 90,
    this is typically \code{TRUE}, otherwise this should probably
    be set to \code{FALSE} (see Examples below).}
  \item{squareShape}{A \code{shape} value to control the behaviour
    of the curve relative to any additional control point that
    is inserted if \code{square} is \code{TRUE}.}
  \item{inflect}{A logical value specifying whether the curve
    should be cut in half and inverted (see Examples below).}
  \item{arrow}{A list describing arrow heads to place at either end
    of the curve, as produced by the \code{arrow} function.}
  \item{debug}{A logical value indicating whether debugging
    information should be drawn.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments to be passed to \code{curveGrob}.}
  \item{theta}{An angle (in degrees).}
}
\value{
  A grob object.
}
\details{
  Both functions create a curve grob (a graphical object describing an
  curve), but only \code{grid.curve}
  draws the curve.

  The \code{arcCurvature} function can be used to calculate a
  \code{curvature} such that control points are generated on
  an arc corresponding to angle \code{theta}.  This is typically
  used in conjunction with a large \code{ncp} to produce a curve
  corresponding to the desired arc.
}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{grid.xspline}},
  \code{\link{arrow}}}
}
\examples{
curveTest <- function(i, j, ...) {
  pushViewport(viewport(layout.pos.col=j, layout.pos.row=i))
  do.call("grid.curve", c(list(x1=.25, y1=.25, x2=.75, y2=.75),
          if (is.null(...)) NULL else list(...)))
  grid.text(sub("list", "", deparse(substitute(list(...)))),
            y=unit(1, "npc"))
  popViewport()
}
# grid.newpage()
pushViewport(plotViewport(c(0, 0, 1, 0),
                          layout=grid.layout(2, 1, heights=c(2, 1))))
pushViewport(viewport(layout.pos.row=1,
                      layout=grid.layout(3, 3, respect=TRUE)))
curveTest(1, 1, NULL)
curveTest(1, 2, inflect=TRUE)
curveTest(1, 3, angle=135)
curveTest(2, 1, arrow=arrow())
curveTest(2, 2, ncp=8)
curveTest(2, 3, shape=0)
curveTest(3, 1, curvature=-1)
curveTest(3, 2, square=FALSE)
curveTest(3, 3, debug=TRUE)
popViewport()
pushViewport(viewport(layout.pos.row=2,
                      layout=grid.layout(3, 3)))
curveTest(1, 1, NULL)
curveTest(1, 2, inflect=TRUE)
curveTest(1, 3, angle=135)
curveTest(2, 1, arrow=arrow())
curveTest(2, 2, ncp=8)
curveTest(2, 3, shape=0)
curveTest(3, 1, curvature=-1)
curveTest(3, 2, square=FALSE)
curveTest(3, 3, debug=TRUE)
popViewport(2)
}
\keyword{dplot}
