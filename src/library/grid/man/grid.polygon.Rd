\name{grid.polygon}
\alias{grid.polygon}
\title{ Draw a Polygon }
\description{
  This function draws a polygon.  The final point will automatically
  be connected to the initial point.
}
\usage{
grid.polygon(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
             default.units="npc",
             gp=gpar(), draw=TRUE, vp=NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-locations.}
  \item{y}{A numeric vector or unit object specifying y-locations.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}  
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  An object of class \code{"grob"}.
}
\details{
  The \code{"grob"} object contains an object of class \code{"polygon"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
