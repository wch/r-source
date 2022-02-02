% File src/library/grid/man/grid.reorder.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.reorder}
\alias{grid.reorder}
\alias{reorderGrob}
\title{ Reorder the children of a gTree }
\description{
  Change the order in which the children of a gTree get drawn.
}
\usage{
grid.reorder(gPath, order, back=TRUE, grep=FALSE, redraw=TRUE)
reorderGrob(x, order, back=TRUE)
}
\arguments{
  \item{gPath}{A gPath object specifying a gTree within the current
    scene.}
  \item{x}{A gTree object to be modified.}
  \item{order}{A character vector or a numeric vector that specifies
    the new drawing order for the children of the gTree.  May not refer
    to all children of the gTree (see Details).}
  \item{back}{Controls what happens when the \code{order} does not
    specify all children of the gTree (see Details).}
  \item{grep}{Should the \code{gPath} be treated as a regular expression?}
  \item{redraw}{Should the modified scene be redrawn?}
}
\details{
  In the simplest case, \code{order} specifies a new ordering for all of
  the children of the gTree.  The children may be specified either by
  name or by existing numerical order.

  If the \code{order} does not
  specify all children of the gTree then, by default, the children
  specified by \code{order} are drawn first and then all remaining
  children are drawn.  If \code{back=FALSE} then the children not
  specified in \code{order} are drawn first, followed by the specified
  children.  This makes it easy to specify a send-to-back or
  bring-to-front reordering.  The \code{order} argument is \emph{always}
  in back-to-front order.

  It is not possible to reorder the grid display list (the top-level
  grobs in the current scene) because the display list is a mixture of
  grobs and viewports (so it is not clear what reordering even means and
  it would be too easy to end up with a scene that would not draw).
  If you want to reorder the grid display list, try \code{grid.grab()}
  to create a gTree and then reorder (and redraw) that gTree.
}
\section{Warning}{
  This function may return a gTree that will not draw.  For example, a
  gTree has two children, A and B (in that order),
  and the width of child B depends on the width of child A (e.g., a box
  around a piece of text).  Switching the order so that B is drawn
  before A will not allow B to be drawn.  If this happens with
  \code{grid.reorder()}, the modification will not be performed.  If
  this happens with \code{reorderGrob()} it should be possible simply to
  restore the original order.
}
\value{
  \code{grid.reorder()} is called for its side-effect of modifying the
  current scene.
  \code{reorderGrob()} returns the modified gTree.
}
\author{ Paul Murrell }
\examples{
# gTree with two children, "red-rect" and "blue-rect" (in that order)
gt <- gTree(children=gList(
                rectGrob(gp=gpar(col=NA, fill="red"),
                         width=.8, height=.2, name="red-rect"),
                rectGrob(gp=gpar(col=NA, fill="blue"),
                         width=.2, height=.8, name="blue-rect")),
            name="gt")
grid.newpage()
grid.draw(gt)
# Spec entire order as numeric (blue-rect, red-rect)
grid.reorder("gt", 2:1)
# Spec entire order as character
grid.reorder("gt", c("red-rect", "blue-rect"))
# Only spec the one I want behind as character
grid.reorder("gt", "blue-rect")
# Only spec the one I want in front as character
grid.reorder("gt", "blue-rect", back=FALSE)
}
\keyword{ dplot }
