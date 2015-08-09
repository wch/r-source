% File src/library/grid/man/grid.bezier.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.bezier}
\alias{grid.bezier}
\alias{bezierGrob}
\title{ Draw a Bezier Curve }
\description{
  These functions create and draw Bezier Curves (a curve drawn
  relative to 4 control points).
}
\usage{
grid.bezier(...)
bezierGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0),
            id = NULL, id.lengths = NULL,
            default.units = "npc", arrow = NULL,
            name = NULL, gp = gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations of
    spline control points.}
  \item{y}{A numeric vector or unit object specifying y-locations of
    spline control points.}
  \item{id}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple beziers.  All locations with the same
    \code{id} belong to the same bezier.}
  \item{id.lengths}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple bezier.  Specifies consecutive blocks of
    locations which make up separate beziers.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y}
    are only given as numeric vectors.}
  \item{arrow}{A list describing arrow heads to place at either end
    of the bezier, as produced by the \code{arrow} function.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments to be passed to \code{bezierGrob}.}
}
\value{
  A grob object.
}
\details{
  Both functions create a beziergrob (a graphical object describing a
  Bezier curve), but only \code{grid.bezier}
  draws the Bezier curve.

  A Bezier curve is a line drawn relative to 4 control points.

  Missing values are not allowed for \code{x} and \code{y}
  (i.e., it is not valid for a control point to be missing).

  The curve is currently drawn using an approximation based on
  X-splines.
}
\seealso{
  \link{Grid},
  \code{\link{viewport}},
  \code{\link{arrow}}.

  \code{\link{grid.xspline}}.
}
\examples{
x <- c(0.2, 0.2, 0.4, 0.4)
y <- c(0.2, 0.4, 0.4, 0.2)

grid.newpage()
grid.bezier(x, y)
grid.bezier(c(x, x + .4), c(y + .4, y + .4),
            id=rep(1:2, each=4))
grid.segments(.4, .6, .6, .6)
grid.bezier(x, y,
            gp=gpar(lwd=3, fill="black"),
            arrow=arrow(type="closed"),
            vp=viewport(x=.9))
}
\keyword{dplot}
