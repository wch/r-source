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

  If the grob describes multiple shapes, in most cases, the boundary
  value will correspond to the edge of a bounding box around all of
  the shapes.  The exception to this is a polygon grob describing
  multiple polygons;  in that case, the edge corresponds to a
  convex hull around all points of all polygons described by the grob.
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
