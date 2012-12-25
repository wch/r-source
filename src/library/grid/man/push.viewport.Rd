% File src/library/grid/man/push.viewport.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{push.viewport}
\alias{push.viewport}
\title{Push a Viewport onto the Grid Viewport Stack}
\description{
  Grid maintains a viewport stack --- a list of nested drawing
  contexts.

  This function makes the specified viewport the default viewport
  and makes its parent the previous default viewport (i.e.,
  nests the specified context within the previous default context).
}
\section{Warning}{
  This function has been deprecated.  Please use \code{pushViewport}
  instead.
}
\usage{
push.viewport(..., recording=TRUE)
}
\arguments{
  \item{\dots}{One or more objects of class \code{"viewport"}, or
    \code{NULL}.}
  \item{recording}{A logical value to indicate whether the set-viewport
    operation should be recorded on the Grid display list.}
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{pop.viewport}}.
}
\keyword{dplot}
