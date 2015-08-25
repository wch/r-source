% File src/library/grid/man/xDetails.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{xDetails}
\alias{xDetails}
\alias{yDetails}
\title{ Boundary of a grid grob }
\description{
  These generic functions are used to determine a location
  on the boundary of a grid grob.
}
\usage{
xDetails(x, theta)
yDetails(x, theta)
}
\arguments{
  \item{x}{ A grid grob. }
  \item{theta}{ A numeric angle, in degrees, measured
    anti-clockwise from the 3 o'clock \emph{or} one of
    the following character strings: \code{"north"},
    \code{"east"}, \code{"west"}, \code{"south"}.}
}
\details{
  The location on the grob boundary is determined by taking
  a line from the centre of the grob at the angle \code{theta}
  and intersecting it with the convex hull of the grob
  (for the basic
  grob primitives, the centre is
  determined as half way between the minimum and maximum values
  in x and y directions).

  These functions are called in the calculation of
  \code{"grobx"} and \code{"groby"} units as produced by
  the \code{grobX} and \code{grobY} functions.
  Methods should be written for classes derived from grob or gTree
  where the boundary of the grob can be determined.
}
\value{
  A unit object.
}
\seealso{
  \code{\link{grobX}},
  \code{\link{grobY}}.
}
\author{ Paul Murrell }
\keyword{ dplot }
