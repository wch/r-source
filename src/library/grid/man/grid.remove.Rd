\name{grid.remove}
\alias{grid.remove}
\alias{removeGrob}
\title{Remove a Grid Graphical Object}
\description{
  Remove a grob from a gTree or a descendant of a gTree.
}
\usage{
grid.remove(gPath, warn=TRUE, strict = FALSE, redraw=TRUE) 
removeGrob(gTree, gPath, warn=TRUE, strict = FALSE) 
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{ A gPath object. For \code{grid.remove} this
    specifyies a gTree on the display list.  For \code{removeGrob} this
    specifies a descendant of the specified gTree. }
  \item{warn}{A logical to indicate whether failing to find the
  specified grob should trigger an error.  }
  \item{strict}{ A boolean indicating whether the gPath must be
    matched exactly. }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
}
\details{
  \code{removeGrob} copies the specified grob and returns a modified
  grob.
  
  \code{grid.remove} destructively modifies a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

}
\value{
  \code{removeGrob} returns a grob object;  \code{grid.remove} returns \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}},
  \code{\link{removeGrob}}, \code{\link{removeGrob}}.
}
\examples{
}
\keyword{dplot}
