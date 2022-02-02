\name{depth}
\alias{depth}
\alias{depth.viewport}
\alias{depth.path}
\title{
  Determine the number of levels in an object.
}
\description{
  Determine the number of levels in a viewport stack or tree,
  in a viewport path, or in a grob path.
}
\usage{
depth(x, ...)
\method{depth}{viewport}(x, ...)
\method{depth}{path}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Typically a viewport or viewport stack or viewport tree or viewport
    list, or a viewport path, or a grob path.
  }
  \item{\dots}{ Arguments used by other methods. }
}
\details{
  Depths of paths are pretty straightforward because they contain
  no branchings.  The depth of a viewport stack is the sum of the depths
  of the components of the stack.  The depth of a viewport tree is
  the depth of the parent plus the depth of the children.
  The depth of a viewport list is the depth of the last component
  of the list.
}
\value{
  An integer value.
}
\seealso{
  \code{\link{viewport}}, \code{\link{vpPath}}, \code{\link{gPath}}.
}
\examples{
vp <- viewport()
depth(vp)
depth(vpStack(vp, vp))
depth(vpList(vpStack(vp, vp), vp))
depth(vpPath("vp"))
depth(vpPath("vp1", "vp2"))
}
\keyword{dplot}
