\name{grid.move.to}
\alias{grid.move.to}
\alias{grid.line.to}
\alias{moveToGrob}
\alias{lineToGrob}
\title{ Move or Draw to a Specified Position }
\description{
  Grid has the notion of a current location.  These functions
  sets that location.
}
\usage{
grid.move.to(x = 0, y = 0, default.units = "npc", name = NULL,
             draw = TRUE, vp = NULL)

moveToGrob(x = 0, y = 0, default.units = "npc", name = NULL, vp = NULL)

grid.line.to(x = 1, y = 1, default.units = "npc", name = NULL,
             gp = gpar(), draw = TRUE, vp = NULL)

lineToGrob(x = 1, y = 1, default.units = "npc", name = NULL,
           gp = gpar(), vp = NULL)
}
\arguments{
  \item{x}{A numeric value or a unit object specifying an x-value.}
  \item{y}{A numeric value or a unit object specifying a y-value.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric values.}  
  \item{name}{ A character identifier. }
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  Both functions create a move.to/line.to grob (a graphical object describing a
  move-to/line-to), but only \code{grid.move.to/line.to()}
  draws the move.to/line.to (and then only if \code{draw} is \code{TRUE}).  
}
\value{
  A move.to/line.to grob.
  \code{grid.move.to/line.to()} returns the value invisibly.
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
pushViewport(viewport(x=0, y=0, w=0.25, h=0.25, just=c("left", "bottom")))
grid.rect()
grid.grill()
grid.line.to(0.5, 0.5)
popViewport()
}
\keyword{dplot}
