\name{grid.current.viewport}
\alias{grid.current.viewport}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Get the Default Grid Viewport}
\description{
  Returns the viewport that Grid is going to draw into.
}
\usage{
current.viewport(vp=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{vp}{A Grid viewport object.}
}
\details{
  This function has two modes:
  \enumerate{
    \item If \code{vp} is not specified, this function
    returns the default Grid viewport;  this is the
    viewport last set by \code{grid.set.viewport}.  
    \item If a value is specified for \code{vp}, this function
    returns that value if it is a valid viewport.  Otherwise it
    returns the default Grid viewport.}
}
\value{
  A Grid viewport object.
}
\author{Paul Murrell}
\seealso{\code{\link{viewport}}}

\examples{
}
\keyword{ dplot }%-- one or more ...
