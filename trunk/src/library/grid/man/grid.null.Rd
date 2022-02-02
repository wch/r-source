% File src/library/grid/man/grid.null.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.null}
\alias{grid.null}
\alias{nullGrob}
\title{ Null Graphical Object }
\description{
  These functions create a NULL graphical object,
  which has zero width, zero height, and draw nothing.
  It can be used as a place-holder or as an invisible
  reference point for other drawing.
}
\usage{
nullGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
         default.units = "npc",
         name = NULL, vp = NULL)
grid.null(...)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments passed to \code{nullGrob()}.}
}
\value{
  A null grob.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\examples{
grid.newpage()
grid.null(name="ref")
grid.rect(height=grobHeight("ref"))
grid.segments(0, 0, grobX("ref", 0), grobY("ref", 0))
}
\keyword{dplot}
