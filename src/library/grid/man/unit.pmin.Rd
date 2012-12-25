% File src/library/grid/man/unit.pmin.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unit.pmin}
\alias{unit.pmin}
\alias{unit.pmax}
\title{ Parallel Unit Minima and Maxima }
\description{
  Returns a unit object whose i'th value is the minimum (or maximum)
  of the i'th values of the arguments.
}
\usage{
unit.pmin(...)
unit.pmax(...)
}
\arguments{
  \item{\dots}{ One or more unit objects. }
}
\details{
  The length of the result is the maximum of the lengths of the
  arguments;  shorter arguments are recycled in the usual manner.
}
\value{
  A unit object.
}
\author{ Paul Murrell }
\examples{
max(unit(1:3, "cm"), unit(0.5, "npc"))
unit.pmax(unit(1:3, "cm"), unit(0.5, "npc"))
}
\keyword{dplot}
