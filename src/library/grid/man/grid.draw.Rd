\name{grid.draw}
\alias{grid.draw}
\title{Draw a Grid Graphical Object}
\description{
  Produces graphical output from a graphical object.
}
\usage{
grid.draw(x, recording=TRUE)
}
\arguments{
  \item{x}{An object of class \code{"grob"} or NULL.}
  \item{recording}{A logical value to indicate whether the drawing
    operation should be recorded on the Grid display list.}
}
\details{
  This function doesn't do much itself beyond some system bookkeeping.

  It calls the generic function \code{draw.details}, dispatching on the
  class of the \code{list.struct} within the graphical object, and
  the actual drawing occurs in the appropriate method.
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}

\examples{
grid.newpage()
## Create a graphical object, but don't draw it
l <- grid.lines(draw=FALSE)
## Draw it
grid.draw(l)
}
\keyword{dplot}
