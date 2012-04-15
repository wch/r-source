% File src/library/grid/man/current.viewport.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Querying the Viewport Tree}
\alias{current.viewport}
\alias{current.vpTree}
\alias{current.vpPath}
\alias{current.transform}
\title{Get the Current Grid Viewport (Tree)}
\description{
  \code{current.viewport()}
  returns the viewport that Grid is going to draw into.

  \code{current.vpTree} returns the entire Grid viewport tree.

  \code{current.vpPath} returns the viewport path to the current viewport.

  \code{current.transform} returns the transformation
  matrix for the current viewport.
}
\usage{
current.viewport(vp=NULL)
current.vpTree(all=TRUE)
current.vpPath()
current.transform()
}
\arguments{
  \item{vp}{A Grid viewport object.  Use of this argument has been deprecated.}
  \item{all}{A logical value indicating whether the entire viewport
    tree should be returned.}
}
\details{
  If \code{all} is \code{FALSE} then
  \code{current.vpTree} only returns the subtree below
  the current viewport.
}
\value{
  A Grid viewport object from \code{current.viewport} or
  \code{current.vpTree}.

  \code{current.transform} returns a 4x4 transformation matrix.

  The viewport path returned by \code{current.vpPath} is \code{NULL}
  if the current viewport is the \code{ROOT} viewport
}
\author{Paul Murrell}
\seealso{
  \code{\link{viewport}}
}
\examples{
grid.newpage()
pushViewport(viewport(width=0.8, height=0.8, name="A"))
pushViewport(viewport(x=0.1, width=0.3, height=0.6,
  just="left", name="B"))
upViewport(1)
pushViewport(viewport(x=0.5, width=0.4, height=0.8,
  just="left", name="C"))
pushViewport(viewport(width=0.8, height=0.8, name="D"))
current.vpPath()
upViewport(1)
current.vpPath()
current.vpTree()
current.viewport()
current.vpTree(all=FALSE)
popViewport(0)
}
\keyword{dplot}
