\name{vpPath}
\alias{vpPath}
\title{ Concatenate Viewport Names }
\description{
  This function can be used to generate a viewport path for use
  in \code{downViewport} or \code{seekViewport}.

  A viewport path is a list of nested viewport names.
}
\usage{
vpPath(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Character values which are viewport names. }
}
\details{
  Viewport names must only be unique amongst viewports which
  share the same parent in the viewport tree.

  This function can be used to generate a specification for
  a viewport that includes the viewport's parent's name
  (and the name of its parent and so on).

  For interactive use, it is possible to directly specify
  a path, but it is strongly recommended that this function
  is used otherwise in case the path separator is changed
  in future versions of grid.
}
\value{
  A \code{ vpPath } object.
}
\seealso{
  \code{\link{viewport}},
  \code{\link{pushViewport}},
  \code{\link{popViewport}},
  \code{\link{downViewport}},
  \code{\link{seekViewport}},
  \code{\link{upViewport}}
}
\examples{
vpPath("vp1", "vp2")
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
