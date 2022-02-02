% File src/library/grid/man/grid.convert.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.convert}
\alias{convertUnit}
\alias{convertX}
\alias{convertY}
\alias{convertWidth}
\alias{convertHeight}
\title{ Convert Between Different grid Coordinate Systems }
\description{
  These functions take a unit object and convert it to an
  equivalent
  unit object in a different coordinate system.
}
\usage{
convertX(x, unitTo, valueOnly = FALSE)
convertY(x, unitTo, valueOnly = FALSE)
convertWidth(x, unitTo, valueOnly = FALSE)
convertHeight(x, unitTo, valueOnly = FALSE)
convertUnit(x, unitTo,
             axisFrom = "x", typeFrom = "location",
             axisTo = axisFrom, typeTo = typeFrom,
             valueOnly = FALSE)
}
\arguments{
  \item{x}{ A unit object. }
  \item{unitTo}{ The coordinate system to convert the unit to.
    See the \code{\link{unit}} function for valid coordinate systems.}
  \item{axisFrom}{ Either \code{"x"} or \code{"y"} to indicate
    whether the unit object represents a value in the x- or
    y-direction. }
  \item{typeFrom}{ Either \code{"location"} or \code{"dimension"}
    to indicate whether the unit object represents a location or a
    length. }
  \item{axisTo}{ Same as \code{axisFrom}, but applies to the unit object
    that is to be created. }
  \item{typeTo}{ Same as \code{typeFrom}, but applies to the unit object
    that is to be created. }
  \item{valueOnly}{ A logical indicating.  If \code{TRUE} then
    the function does not return a unit object, but rather only the
    converted numeric values. }
}
\details{
  The \code{convertUnit} function allows for general-purpose
  conversions.  The other four functions are just more convenient
  front-ends to it for the most common conversions.

  The conversions occur within the current viewport.

  It is not currently possible to convert to all valid coordinate systems
  (e.g., "strwidth" or "grobwidth").  I'm not sure if all of these
  are impossible, they just seem implausible at this stage.

  In normal usage of grid, these functions should not be necessary.
  If you want to express a location or dimension in inches rather
  than user coordinates then you should simply do something like
  \code{unit(1, "inches")} rather than something like
  \code{unit(0.134, "native")}.

  In some cases, however, it is necessary for the user to
  perform calculations on a unit value and this function becomes
  necessary.  In such cases, please take note of the warning below.
}
\value{
  A unit object in the specified coordinate system
  (unless \code{valueOnly} is \code{TRUE} in which case
  the returned value is a numeric).
}
\author{ Paul Murrell }

\section{Warning}{
  The conversion is only valid for the current device size.
  If the device is resized then at least some conversions will
  become invalid.  For example, suppose that I create a unit
  object as follows: \code{oneinch <- convertUnit(unit(1, "inches"),
    "native")}.  Now if I resize the device, the unit object in
  oneinch no longer corresponds to a physical length of 1 inch.
  }
\seealso{ \code{\link{unit}} }
\examples{
## A tautology
convertX(unit(1, "inches"), "inches")
## The physical units
convertX(unit(2.54, "cm"), "inches")
convertX(unit(25.4, "mm"), "inches")
convertX(unit(72.27, "points"), "inches")
convertX(unit(1/12*72.27, "picas"), "inches")
convertX(unit(72, "bigpts"), "inches")
convertX(unit(1157/1238*72.27, "dida"), "inches")
convertX(unit(1/12*1157/1238*72.27, "cicero"), "inches")
convertX(unit(65536*72.27, "scaledpts"), "inches")
convertX(unit(1/2.54, "inches"), "cm")
convertX(unit(1/25.4, "inches"), "mm")
convertX(unit(1/72.27, "inches"), "points")
convertX(unit(1/(1/12*72.27), "inches"), "picas")
convertX(unit(1/72, "inches"), "bigpts")
convertX(unit(1/(1157/1238*72.27), "inches"), "dida")
convertX(unit(1/(1/12*1157/1238*72.27), "inches"), "cicero")
convertX(unit(1/(65536*72.27), "inches"), "scaledpts")

pushViewport(viewport(width=unit(1, "inches"),
                       height=unit(2, "inches"),
                       xscale=c(0, 1),
                       yscale=c(1, 3)))
  ## Location versus dimension
  convertY(unit(2, "native"), "inches")
  convertHeight(unit(2, "native"), "inches")
  ## From "x" to "y" (the conversion is via "inches")
  convertUnit(unit(1, "native"), "native",
               axisFrom="x", axisTo="y")
  ## Convert several values at once
  convertX(unit(c(0.5, 2.54), c("npc", "cm")),
                c("inches", "native"))
popViewport()
## Convert a complex unit
convertX(unit(1, "strwidth", "Hello"), "native")
}
\keyword{dplot}
