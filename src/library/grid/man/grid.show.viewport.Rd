\name{grid.show.viewport}
\alias{grid.show.viewport}
\title{Draw a Diagram of a Grid Viewport}
\description{
  This function uses Grid graphics to draw a diagram of
  a Grid viewport.
}
\usage{
grid.show.viewport(v, parent.layout = NULL, newpage = TRUE,
                   border.fill="light grey",
                   vp.col="blue", vp.fill="light blue",
                   scale.col="red",
                   vp = NULL)
}
\arguments{
  \item{v}{A Grid viewport object.}
  \item{parent.layout}{A grid layout object.  If this is not NULL and
    the viewport given in \code{v} has its location specified relative
    to the layout, then the diagram shows the layout and which cells
    \code{v} occupies within the layout.}
  \item{newpage}{A logical value to indicate whether to move to
    a new page before drawing the diagram.}
  \item{border.fill}{Colour to fill the border margin.}
  \item{vp.col}{Colour for the border of the viewport region.}
  \item{vp.fill}{Colour to fill the viewport region.}
  \item{scale.col}{Colour to draw the viewport axes.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\details{
  A viewport is created within \code{vp} to provide a margin for
  annotation, and the diagram is drawn within that new viewport.
  By default, the margin is filled with light grey, the new viewport is filled
  with white and framed with a black border, and the viewport region
  is filled with light blue and framed with a blue border.
  The diagram is annotated with the width and height (including units)
  of the viewport, the (x, y) location of the viewport, and
  the x- and y-scales of the viewport, using red lines and text.
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}

\examples{
## Diagram of a sample viewport
grid.show.viewport(viewport(x=0.6, y=0.6,
                            w=unit(1, "inches"), h=unit(1, "inches")))
grid.show.viewport(viewport(layout.pos.row=2, layout.pos.col=2:3),
                   grid.layout(3, 4))
}
\keyword{dplot}
