% File src/library/grid/man/convertNative.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{convertNative}
\alias{convertNative}
\title{ Convert a Unit Object to Native units }
\description{
  \bold{This function is deprecated in grid version 0.8 and will be
    made defunct in grid version 1.9}

  You should use the \code{convertUnit()} function or one of its
  close allies instead.

  This function returns a numeric vector containing the specified
  x or y locations or dimensions, converted to "user" or "data"
  units, relative to the current viewport.
}
\usage{
convertNative(unit, dimension="x", type="location")
}
\arguments{
  \item{unit}{ A unit object. }
  \item{dimension}{ Either "x" or "y". }
  \item{type}{ Either "location" or "dimension". }
}
\value{
  A numeric vector.
}
\author{ Paul Murrell }
\section{WARNING }{ If you draw objects based on output from these conversion
    functions, then resize your device, the objects will be drawn
    incorrectly -- the base R display list will not recalculate these
    conversions.  This means that you can only rely on the results of
    these calculations if the size of your device is fixed.}
\seealso{ \code{\link{grid.convert}}, \code{\link{unit}} }

\examples{
grid.newpage()
pushViewport(viewport(width=unit(.5, "npc"),
                       height=unit(.5, "npc")))
grid.rect()
w <- convertNative(unit(1, "inches"))
h <- convertNative(unit(1, "inches"), "y")
# This rectangle starts off life as 1in square, but if you
# resize the device it will no longer be 1in square
grid.rect(width=unit(w, "native"), height=unit(h, "native"),
          gp=gpar(col="red"))
popViewport(1)

# How to use grid.convert(), etc instead
convertNative(unit(1, "inches")) ==
  convertX(unit(1, "inches"), "native", valueOnly=TRUE)
convertNative(unit(1, "inches"), "y", "dimension") ==
  convertHeight(unit(1, "inches"), "native", valueOnly=TRUE)
}
\keyword{ dplot }
