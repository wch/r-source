\name{grid.rect}
\alias{grid.rect}
\title{Draw a rectangle in a Grid Viewport}
\description{
  This function draws a rectangle.
}
\usage{
grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
      width = unit(1, "npc"), height = unit(1, "npc"),
      just = "centre", default.units = "npc", 
      gp=gpar(), draw = TRUE, vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{width}{A numeric vector or unit object specifying width.}
  \item{height}{A numeric vector or unit object specifying height.}
  \item{just}{The justification of the rectangle about its (x, y)
    location.  If two values are given, the first specifies
    horizontal justification and the second specifies vertical
    justification.}
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
  The \code{"grob"} object contains an object of class \code{"rect"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
