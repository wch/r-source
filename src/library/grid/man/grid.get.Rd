\name{grid.get}
\alias{grid.get}
\alias{getGrob}
\title{Get a Grid Graphical Object}
\description{
  Retrieve a grob or a descendant of a grob.
}
\usage{
grid.get(gPath, strict = FALSE) 
getGrob(gTree, gPath, strict = FALSE) 
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{ A gPath object. For \code{grid.get} this
    specifyies a grob on the display list.  For \code{getGrob} this
    specifies a descendant of the specified gTree. }
  \item{strict}{ A boolean indicating whether the gPath must be
    matched exactly. }
}
\value{
  A grob object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}},
  \code{\link{addGrob}}, \code{\link{removeGrob}}.
}
\examples{
grid.xaxis(name="xa")
grid.get("xa")
grid.get(gPath("xa", "ticks"))

grid.draw(gTree(name="gt", children=gList(xaxisGrob(name="axis"))))
grid.get(gPath("gt", "axis", "ticks"))
}
\keyword{dplot}
