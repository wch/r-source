\name{current.viewport}
\alias{current.viewport}
\title{Get the Default Grid Viewport}
\description{
  Returns the viewport that Grid is going to draw into.
}
\usage{
current.viewport(vp=NULL)
}
\arguments{
  \item{vp}{A Grid viewport object.}
}
\details{
  This function should only be used without the \code{vp}
  argument (i.e., only to return the current viewport).

  The \code{vp} argument only exists for historical reasons.
  It will be removed in future versions.
}
\value{
  A Grid viewport object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{viewport}}
}
\keyword{dplot}
