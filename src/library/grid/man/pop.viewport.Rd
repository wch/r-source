% File src/library/grid/man/pop.viewport.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{pop.viewport}
\alias{pop.viewport}
\title{Pop a Viewport off the Grid Viewport Stack}
\description{
  Grid maintains a viewport stack --- a list of nested drawing
  contexts.

  This function makes the parent of the specified viewport the
  new default viewport.
}
\section{Warning}{
  This function has been deprecated.  Please use \code{popViewport}
  instead.
}
\usage{
pop.viewport(n=1, recording=TRUE)
}
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
\keyword{dplot}
