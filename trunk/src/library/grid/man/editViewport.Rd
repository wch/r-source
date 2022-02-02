% File src/library/grid/man/editViewport.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{editViewport}
\alias{editViewport}
\title{ Modify a Viewport }
\description{
  This is a convenience function for producing a new viewport
  from a copy of an existing viewport (by default the current
  viewport), with specified modifications.
}
\usage{
editViewport(vp=current.viewport(), ...)
}
\arguments{
  \item{vp}{ A viewport object. }
  \item{\dots}{ Modification of the viewport
    (should all be valid arguments to
    the \code{viewport()} function. }
}
\value{
  A grid viewport object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{viewport}}.
}
\keyword{dplot}
