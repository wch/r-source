\name{grid.grab}
\alias{grid.grab}
\title{ Grab the current grid output }
\description{
  Creates a gTree object from the current grid display list.
}
\usage{
grid.grab(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ arguments passed to gTree, for example, a
    name and/or class for the gTree that is created.}
}
\details{
  The gTree has a childrenvp slot containing all viewports on
  the display list (including those
  that are popped).  
  Every grob on the display list is made a child of the new
  gTree, with a vpPath in the vp slot so that the grob is
  drawn in the appropriate viewport.

  In other words, the gTree contains all elements on the display
  list, but in a slightly altered form.

  The gTree should produce exactly the same output as the
  display list.
}
\value{
  A gTree object.
}
\seealso{ \code{\link{gTree}}
}
\examples{
pushViewport(viewport(w=.5, h=.5))
grid.rect()
grid.points(runif(10), runif(10))
popViewport()
grab <- grid.grab()
grid.newpage()
grid.draw(grab)
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
