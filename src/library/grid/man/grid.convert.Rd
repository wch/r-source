\name{grid.convert}
\alias{grid.convert}
\alias{grid.convertX}
\alias{grid.convertY}
\alias{grid.convertWidth}
\alias{grid.convertHeight}
\title{ Convert Between Different grid Coordinate Systems }
\description{
  These functions take a unit object and convert it to an
  equivalent
  unit object in a different coordinate system.
}
\usage{
grid.convertX(x, unitTo, valueOnly = FALSE) 
grid.convertY(x, unitTo, valueOnly = FALSE) 
grid.convertWidth(x, unitTo, valueOnly = FALSE) 
grid.convertHeight(x, unitTo, valueOnly = FALSE) 
grid.convert(x, unitTo,
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
  The \code{grid.convert()} function allows for general-purpose
  conversions.  The other four functions are just more convenient
  front-ends to it for the most common conversions.
  
  The conversions occur within the current viewport.
  
  It is not currently possible to convert to all valid coordinate systems
  (e.g., "strwidth" or "grobwidth").  I'm not sure if all of these
  are impossible, they just seem implausible at this stage.

  In normal usage of grid, this function should not be necessary.
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
  the returned value is a numeric.
}
\author{ Paul Murrell }

\section{Warning}{
  The conversion is only valid for the current device size.
  If the device is resized then at least some conversions will
  become invalid.  For example, suppose that I create a unit
  object as follows: \code{oneinch <- grid.convert(unit(1, "inches"),
    "native"}.  Now if I resize the device, the unit object in
  oneinch no longer corresponds to a physical length of 1 inch.
  }
\seealso{ \code{\link{unit}} }
\examples{
## A tautology
grid.convertX(unit(1, "inches"), "inches")
## The physical units
grid.convertX(unit(2.54, "cm"), "inches")
grid.convertX(unit(25.4, "mm"), "inches")
grid.convertX(unit(72.27, "points"), "inches")
grid.convertX(unit(1/12*72.27, "picas"), "inches")
grid.convertX(unit(72, "bigpts"), "inches")
grid.convertX(unit(1157/1238*72.27, "dida"), "inches")
grid.convertX(unit(1/12*1157/1238*72.27, "cicero"), "inches")
grid.convertX(unit(65536*72.27, "scaledpts"), "inches")
push.viewport(viewport(width=unit(1, "inches"),
                       height=unit(2, "inches"),
                       xscale=c(0, 1),
                       yscale=c(1, 3)))
  ## Location versus dimension
  grid.convertY(unit(2, "native"), "inches")
  grid.convertHeight(unit(2, "native"), "inches")
  ## From "x" to "y" (the conversion is via "inches")
  grid.convert(unit(1, "native"), "native",
               axisFrom="x", axisTo="y")
  ## Convert several values at once
  grid.convertX(unit(c(0.5, 2.54), c("npc", "cm")),
                c("inches", "native"))
pop.viewport()
## Convert a complex unit
grid.convertX(unit(1, "strwidth", "Hello"), "native")
}
\keyword{dplot}
