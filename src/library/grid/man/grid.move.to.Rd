\name{grid.move.to}
\alias{grid.move.to}
\alias{grid.line.to}
\title{ Move to a Specified Position }
\description{
  Grid has the notion of a current location.  This function
  sets that location.
}
\usage{
grid.move.to(x=0, y=0, default.units="npc", draw=TRUE, vp=NULL)
grid.line.to(x=1, y=1, default.units="npc", draw=TRUE, gp=gpar(), vp=NULL)
}
\arguments{
  \item{x}{A numeric value or a unit object specifying an x-value.}
  \item{y}{A numeric value or a unit object specifying a y-value.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric values.}  
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}

\examples{
grid.newpage()
grid.move.to(0.5, 0.5)
grid.line.to(1, 1)
grid.line.to(0.5, 0)
push.viewport(viewport(x=0, y=0, w=0.25, h=0.25, just=c("left", "bottom")))
grid.rect()
grid.grill()
grid.line.to(0.5, 0.5)
pop.viewport()
}
\keyword{dplot}
