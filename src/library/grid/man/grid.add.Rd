\name{grid.add}
\alias{grid.add}
\alias{addGrob}
\title{Add a Grid Graphical Object}
\description{
  Add a grob to a gTree or a descendant of a gTree.
}
\usage{
grid.add(gPath, child, strict = FALSE, redraw=TRUE) 
addGrob(gTree, child, gPath = NULL, strict = FALSE) 
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{ A gPath object. For \code{grid.add} this
    specifyies a gTree on the display list.  For \code{addGrob} this
    specifies a descendant of the specified gTree. }
  \item{child}{ A grob object. }
  \item{strict}{ A boolean indicating whether the gPath must be
    matched exactly. }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
}
\details{
  \code{addGrob} copies the specified grob and returns a modified
  grob.
  
  \code{grid.add} destructively modifies a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

}
\value{
  \code{addGrob} returns a grob object;  \code{grid.add} returns \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}},
  \code{\link{addGrob}}, \code{\link{removeGrob}}.
}
\examples{
}
\keyword{dplot}
