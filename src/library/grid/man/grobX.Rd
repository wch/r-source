% File src/library/grid/man/grobX.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grobX}
\alias{grobX}
\alias{grobY}
\title{Create a Unit Describing a Grob Boundary Location}
\description{
  These functions create a unit object describing a location somewhere
  on the boundary of
  a grob.  They are generic.
}
\usage{
grobX(x, theta)
grobY(x, theta)
}
\arguments{
  \item{x}{A grob, or gList, or gTree, or gPath.}
  \item{theta}{An angle indicating where the location is
    on the grob boundary.  Can be one of \code{"east"},
    \code{"north"}, \code{"west"}, or \code{"south"},
    which correspond to angles 0, 90, 180, and 270,
    respectively.}
}
\details{
  The angle is anti-clockwise with zero corresponding to a line
  with an origin centred between the extreme
  points of the shape, and pointing at 3 o'clock.

  If the grob describes a single shape, the boundary value should
  correspond to the exact edge of the shape.

  If the grob describes multiple shapes, the boundary
  value will either correspond to the edge of a bounding box around all of
  the shapes described by the grob (for multiple rectangles, circles,
  xsplines, or text), or to a
  convex hull around all vertices of all shapes described by the grob
  (for multiple polygons, points, lines, polylines,
  and segments).

  Points grobs are currently a special case because the convex hull
  is based on the data symbol \emph{locations} and does not take
  into account the extent of the data symbols themselves.

  The extents of any arrow heads are currently \emph{not} taken into account.
}
\value{
  A unit object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}} and
  \code{\link{grobWidth}}
}
\keyword{dplot}
