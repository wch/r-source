% File src/library/grid/man/arrow.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{arrow}
\alias{arrow}
\title{ Describe arrows to add to a line. }
\description{
  Produces a description of what arrows to add to a line.
  The result can be passed to a function that draws a line,
  e.g., \code{grid.lines}.
}
\usage{
arrow(angle = 30, length = unit(0.25, "inches"),
      ends = "last", type = "open")
}
\arguments{
  \item{angle}{The angle of the arrow head in degrees (smaller numbers
    produce narrower, pointier arrows).  Essentially describes
    the width of the arrow head.}
  \item{length}{A unit specifying the length of the arrow head (from tip
    to base).}
  \item{ends}{One of \code{"last"}, \code{"first"}, or \code{"both"},
    indicating which ends of the line to draw arrow heads.}
  \item{type}{One of \code{"open"} or \code{"closed"} indicating
    whether the arrow head should be a closed triangle.}
}
\examples{
arrow()
}
\keyword{dplot}
