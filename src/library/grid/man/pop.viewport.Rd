\name{pop.viewport}
\alias{pop.viewport}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Pop a Viewport off the Grid Viewport Stack}
\description{
  Grid maintains a viewport stack - a list of nested drawing
  contexts.
  
  This function makes the parent of the specified viewport the
  new default viewport.
}
\usage{
pop.viewport(n=1, recording=TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{n}{An integer giving the number of viewports to pop.
    Defaults to 1.}
  \item{recording}{A logical value to indicate whether the set-viewport
    operation should be recorded on the Grid display list.}
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{push.viewport}}.
}

\examples{
}
\keyword{ dplot }%-- one or more ...
