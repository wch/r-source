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
        default.units = "null", respect = FALSE)
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
  \item{respect}{A logical value indicating whether
    row heights and column widths should respect each other.}
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
}
\keyword{dplot}
