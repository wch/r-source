\name{grid.rect}
\alias{grid.rect}
\alias{rectGrob}
\title{Draw rectangles }
\description{
  These functions create and draw rectangles.
}
\usage{
grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
      width = unit(1, "npc"), height = unit(1, "npc"),
      just = "centre", default.units = "npc", name = NULL,
      gp=gpar(), draw = TRUE, vp = NULL)
rectGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
      width = unit(1, "npc"), height = unit(1, "npc"),
      just = "centre", default.units = "npc", name = NULL,
      gp=gpar(), vp = NULL)
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
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a rect grob (a graphical object describing
  rectangles), but only \code{grid.rect}
  draws the rectangles (and then only if \code{draw} is \code{TRUE}).
  
}
\value{
  A rect grob.  \code{grid.rect} returns the value invisibly.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
