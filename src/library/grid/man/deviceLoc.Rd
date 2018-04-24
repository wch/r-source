% File src/library/grid/man/deviceLoc.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{deviceLoc}
\alias{deviceLoc}
\alias{deviceDim}
\title{ Convert Viewport Location to Device Location }
\description{
  These functions take a pair of unit objects and convert
  them to a pair of device locations (or dimensions) in inches.
}
\usage{
deviceLoc(x, y, valueOnly = FALSE)
deviceDim(w, h, valueOnly = FALSE)
}
\arguments{
  \item{x, y, w, h}{ A unit object. }
  \item{valueOnly}{ A logical indicating.  If \code{TRUE} then
    the function does not return a unit object, but rather only the
    converted numeric values. }
}
\details{
  These functions differ from the functions like \code{convertX()}
  because they convert from the coordinate systems within a viewport
  to inches on the device (i.e., from one viewport to another) and
  because they only deal with pairs of values (locations or dimensions).

  The functions like \code{convertX()} convert between different units within
  the same viewport and convert along a single dimension.
}
\value{
  A list with two components, both of which are unit object in inches
  (unless \code{valueOnly} is \code{TRUE} in which case
  both components are numeric).
}
\author{ Paul Murrell }

\section{Warning}{
  The conversion is only valid for the current device size.
  If the device is resized then at least some conversions will
  become invalid.

  Furthermore, the returned value only makes sense with respect to
  the entire device (i.e., within the context of the root viewport).
  }
\seealso{ \code{\link{unit}} }
\examples{
## A tautology
grid.newpage()
pushViewport(viewport())
deviceLoc(unit(1, "inches"), unit(1, "inches"))

## Something less obvious
grid.newpage()
pushViewport(viewport(width=.5, height=.5))
grid.rect()
x <- unit(1, "in")
y <- unit(1, "in")
grid.circle(x, y, r=unit(2, "mm"))
loc <- deviceLoc(x, y)
loc
upViewport()
grid.circle(loc$x, loc$y, r=unit(1, "mm"), gp=gpar(fill="black"))

## Something even less obvious
grid.newpage()
pushViewport(viewport(width=.5, height=.5, angle=30))
grid.rect()
x <- unit(.2, "npc")
y <- unit(2, "in")
grid.circle(x, y, r=unit(2, "mm"))
loc <- deviceLoc(x, y)
loc
upViewport()
grid.circle(loc$x, loc$y, r=unit(1, "mm"), gp=gpar(fill="black"))
}
\keyword{dplot}
