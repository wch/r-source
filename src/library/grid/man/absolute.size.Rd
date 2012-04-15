% File src/library/grid/man/absolute.size.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{absolute.size}
\alias{absolute.size}
\title{ Absolute Size of a Grob }
\description{
  This function converts a unit object into absolute units.
  Absolute units are unaffected, but non-absolute units are
  converted into \code{"null"} units.
}
\usage{
absolute.size(unit)
}
\arguments{
  \item{unit}{ An object of class \code{"unit"}. }
}
\details{
  Absolute units are things like \code{"inches"}, \code{"cm"},
  and \code{"lines"}.  Non-absolute units are \code{"npc"}
  and \code{"native"}.

  This function is designed to be used in \code{widthDetails}
  and \code{heightDetails} methods.
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{widthDetails}}
  and \code{\link{heightDetails}} methods.
}
\keyword{ dplot }
