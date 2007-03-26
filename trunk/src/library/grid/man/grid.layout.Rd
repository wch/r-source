\name{grid.layout}
\alias{grid.layout}
\title{Create a Grid Layout}
\description{
  This function returns a Grid layout, which describes a subdivision
  of a rectangular region.
}
\usage{
grid.layout(nrow = 1, ncol = 1,
        widths = unit(rep(1, ncol), "null"),
        heights = unit(rep(1, nrow), "null"),
        default.units = "null", respect = FALSE,
        just="centre")
}
\arguments{
  \item{nrow}{An integer describing the number of rows in the layout.}
  \item{ncol}{An integer describing the number of columns in the layout.}
  \item{widths}{A numeric vector or unit object
    describing the widths of the columns
    in the layout.}
  \item{heights}{A numeric vector or unit object
    describing the heights of the rows
    in the layout.}
  \item{default.units}{A string indicating the default units to use
    if \code{widths} or \code{heights} are only given as numeric vectors.}  
  \item{respect}{A logical value or a numeric matrix.
    If a logical, this indicates whether
    row heights and column widths should respect each other.
    If a matrix, non-zero values indicate that the corresponding
    row and column should be respected (see examples below).
  }
  \item{just}{A string vector indicating how the layout should be
    justified if it is not the same size as its parent viewport.
    If there are two values, the first
    value specifies horizontal justification and the second value specifies
    vertical justification.  Possible values are: \code{"left"},
    \code{"right"}, \code{"centre"}, \code{"center"}, \code{"bottom"},
    and \code{"top"}.  NOTE that in this context,
    \code{"left"}, for example, means align the left
    edge of the left-most layout column with the left edge of the
    parent viewport.}
}
\details{
  The unit objects given for the \code{widths} and \code{heights}
  of a layout may use a special \code{units} that only has
  meaning for layouts.  This is the \code{"null"} unit, which
  indicates what relative fraction of the available width/height the
  column/row occupies.  See the reference for a better description
  of relative widths and heights in layouts.
}
\section{WARNING}{
  This function must NOT be confused with the base R graphics function
  \code{layout}.  In particular, do not use \code{layout} in
  combination with Grid graphics.  The documentation for
  \code{layout} may provide some useful information and this
  function should behave identically in comparable situations.  The
  \code{grid.layout} 
  function has \emph{added} the ability to specify a broader range
  of units for row heights and column widths, and allows for nested
  layouts (see \code{viewport}).
}
\value{
  A Grid layout object.
}
\references{Murrell, P. R. (1999), Layouts: A Mechanism for Arranging
  Plots on a Page, \emph{Journal of Computational and Graphical
  Statistics}, \bold{8}, 121--134.}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{grid.show.layout}},
  \code{\link{viewport}},
  \code{\link{layout}}}

\examples{
## A variety of layouts (some a bit mid-bending ...)
layout.torture()
## Demonstration of layout justification
grid.newpage()
testlay <- function(just="centre") {
  pushViewport(viewport(layout=grid.layout(1, 1, widths=unit(1, "inches"),
                          height=unit(0.25, "npc"),
                          just=just)))
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
  grid.rect()
  grid.text(paste(just, collapse="-"))
  popViewport(2)
}
testlay()
testlay(c("left", "top"))
testlay(c("right", "top"))
testlay(c("right", "bottom"))
testlay(c("left", "bottom"))
testlay(c("left"))
testlay(c("right"))
testlay(c("bottom"))
testlay(c("top"))
}
\keyword{dplot}
