\name{grid.polygon}
\alias{grid.polygon}
\alias{polygonGrob}
\title{ Draw a Polygon }
\description{
  These functions create and draw a polygon.
  The final point will automatically
  be connected to the initial point.
}
\usage{
grid.polygon(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
             id=NULL, id.lengths=NULL,
             default.units="npc", name=NULL,
             gp=gpar(), draw=TRUE, vp=NULL)
polygonGrob(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
             id=NULL, id.lengths=NULL,
             default.units="npc", name=NULL,
             gp=gpar(), vp=NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations.}
  \item{y}{A numeric vector or unit object specifying y-locations.}
  \item{id}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple polygons.  All locations with the same
    \code{id} belong to the same polygon.}
  \item{id.lengths}{A numeric vector used to separate locations in \code{x} and
    \code{y} into multiple polygons.  Specifies consecutive blocks of
    locations which make up separate polygons.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}  
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  A grob object.
}
\details{
  Both functions create a polygon grob (a graphical object describing a
  polygon), but only \code{grid.polygon}
  draws the polygon (and then only if \code{draw} is \code{TRUE}).  
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\examples{
grid.polygon()
# Using id (NOTE: locations are not in consecutive blocks)
grid.newpage()
grid.polygon(x=c((0:4)/10, rep(.5, 5), (10:6)/10, rep(.5, 5)),
             y=c(rep(.5, 5), (10:6/10), rep(.5, 5), (0:4)/10),
             id=rep(1:5, 4),
             gp=gpar(fill=1:5))
# Using id.lengths
grid.newpage()
grid.polygon(x=outer(c(0, .5, 1, .5), 5:1/5),
             y=outer(c(.5, 1, .5, 0), 5:1/5),
             id.lengths=rep(4, 5),
             gp=gpar(fill=1:5))
}
\keyword{dplot}
