\name{viewport}
\alias{viewport}
\title{Create a Grid Viewport}
\description{
  This function creates a viewport, which describes a rectangular region
  on a graphics device and defines a number of coordinate systems within
  that region.
}
\usage{
viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
         width = unit(1, "npc"), height = unit(1, "npc"),
         default.units = "npc", just = "centre", 
         gp = gpar(), clip = FALSE,
         xscale = c(0, 1), yscale = c(0, 1),
         angle = 0,
         layout = NULL, layout.pos.row = NULL, layout.pos.col = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{width}{A numeric vector or unit object specifying width.}
  \item{height}{A numeric vector or unit object specifying height.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}  
  \item{just}{A string vector specifying the justification of the viewport
    relative to its (x, y) location.  If there are two values, the first
    value specifes horizontal justification and the second value specifies
    vertical justification.  Possible values are: \code{"left"},
    \code{"right"}, \code{"centre"}, \code{"center"}, \code{"bottom"},
    and \code{"top"}.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{clip}{A logical flag indicating whether to clip to the
    extent of the viewport.}
  \item{xscale}{A numeric vector of length two indicating the minimum and
    maximum on the x-scale.}
  \item{yscale}{A numeric vector of length two indicating the minimum
    and maximum on the y-scale.}
  \item{angle}{A numeric value indicating the angle of rotation of the
    viewport.  Positive values indicate the amount of rotation, in
    degrees, anitclockwise from the positive x-axis.}
  \item{layout}{A Grid layout object which splits the viewport into
    subregions.}
  \item{layout.pos.row}{A numeric vector giving the
    rows occupied by this viewport in its
    parent's layout.}
  \item{layout.pos.col}{A numeric vector giving the
    columns occupied by this viewport in its
    parent's layout.}
}
\details{
  The location and size of a viewport are relative to the coordinate
  systems defined by the viewport's parent (either a graphical device
  or another viewport).  The location and size can be specified in a
  very flexible way by specifying them with unit objects.
  When specifying the location of a viewport, specifying
  both \code{layout.pos.row} and \code{layout.pos.col} as \code{NULL}
  indicates that
  the viewport ignores its parent's layout and specifies its own
  location and size (via its \code{locn}).  If only one of
  \code{layout.pos.row} and \code{layout.pos.col} is \code{NULL}, this
  means occupy ALL of the appropriate row(s)/column(s).  For example,
  \code{layout.pos.row = 1} and \code{layout.pos.col = NULL} means
  occupy all of row 1.  Specifying non-\code{NULL} values for both
  \code{layout.pos.row} and \code{layout.pos.col} means occupy the
  intersection of the appropriate rows and columns.  If a vector of
  length two is
  specified for \code{layout.pos.row} or \code{layout.pos.col}, this
  indicates a range of rows or columns to occupy.  For example,
  \code{layout.pos.row = c(1, 3)} and \code{layout.pos.col = c(2, 4)}
  means occupy cells in the intersection of rows 1, 2, and 3, and
  columns, 2, 3, and 4.

    Clipping obeys only the most recent viewport clip setting.
    For example, if you clip to viewport1, then clip to viewport2,
    the clipping region is determined wholly by viewport2, the
    size and shape of viewport1 is irrelevant (until viewport2
    is popped of course).

    If a viewport is rotated (because of its own \code{angle} setting
    or because it is within another viewport which is rotated) then
    the \code{clip} flag is ignored.  
}
\value{
  An R object of class \code{viewport}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{unit}},
  \code{\link{grid.layout}},
  \code{\link{grid.show.layout}}.
}

\examples{
# Diagram of a sample viewport
grid.show.viewport(viewport(x=0.6, y=0.6,
                   w=unit(1, "inches"), h=unit(1, "inches")))
# Demonstrate viewport clipping
clip.demo <- function(i, j, clip1, clip2, title) {
  push.viewport(viewport(layout.pos.col=i,
                         layout.pos.row=j))
  push.viewport(viewport(width=0.6, height=0.6, clip=clip1))
  grid.rect(gp=gpar(fill="white"))
  grid.circle(r=0.55, gp=gpar(col="red", fill="pink"))
  pop.viewport()
  push.viewport(viewport(width=0.6, height=0.6, clip=clip2))
  grid.polygon(x=c(0.5, 1.1, 0.6, 1.1, 0.5, -0.1, 0.4, -0.1),
               y=c(0.6, 1.1, 0.5, -0.1, 0.4, -0.1, 0.5, 1.1),
               gp=gpar(col="blue", fill="light blue"))
  pop.viewport(2)
}

grid.newpage()
grid.rect(gp=gpar(fill="grey"))
push.viewport(viewport(layout=grid.layout(2, 2)))
clip.demo(1, 1, FALSE, FALSE)
clip.demo(1, 2, TRUE, FALSE)
clip.demo(2, 1, FALSE, TRUE)
clip.demo(2, 2, TRUE, TRUE)
pop.viewport()
}
\keyword{dplot}
