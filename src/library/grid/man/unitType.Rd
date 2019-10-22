% File src/library/grid/man/unitType.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unitType}
\alias{unitType}
\title{Return the Units of a Unit Object}
\description{
  This function returns the units of a unit object.
}
\usage{
unitType(x)
}
\arguments{
  \item{x}{A unit object.}
}
\value{
  For simple units, this will be just a vector of coordinate systems,
  like \code{"inches"} or \code{"npc"}.

  More complex units that involve
  an operation on units return
  an operator, like \code{"sum"}, \code{"min"}, or \code{"max"}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}}
}

\examples{
unitType(unit(1, "npc"))
unitType(unit(1:3/4, "npc"))
unitType(unit(1:3/4, "npc") + unit(1, "inches"))
unitType(min(unit(0.5, "npc"), unit(1, "inches")))
unitType(unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
                unit(1, "strwidth", "hi there")))
}
\keyword{dplot}
