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
unitType(x, recurse = FALSE)
}
\arguments{
  \item{x}{A unit object.}
  \item{recurse}{Whether to recurse into complex units.}
}
\value{
  For simple units, this will be just a vector of coordinate systems,
  like \code{"inches"} or \code{"npc"}.

  More complex units that involve
  an operation on units return
  an operator, like \code{"sum"}, \code{"min"}, or \code{"max"}.

  When \code{recurse = TRUE}, the result is always a list and
  more complex units generate sublists (see the Examples below).
}
\author{Thomas Lin Pedersen and Paul Murrell}
\seealso{
  \code{\link{unit}}
}

\examples{
u <- unit(1:5, c("cm", "mm", "in", "pt", "null"))

unitType(u)
unitType(unit(1, "npc"))
unitType(unit(1:3/4, "npc"))
unitType(unit(1:3/4, "npc") + unit(1, "inches"))
unitType(min(unit(0.5, "npc"), unit(1, "inches")))
unitType(unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
                unit(1, "strwidth", "hi there")))
unitType(min(unit(1, "in"), unit(1, "npc") + unit(1, "mm")))

unitType(u, recurse=TRUE)
unitType(unit(1, "npc"), recurse=TRUE)
unitType(unit(1:3/4, "npc"), recurse=TRUE)
unitType(unit(1:3/4, "npc") + unit(1, "inches"), recurse=TRUE)
unitType(min(unit(0.5, "npc"), unit(1, "inches")), recurse=TRUE)
unitType(unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
                unit(1, "strwidth", "hi there")), recurse=TRUE)
unitType(min(unit(1, "in"), unit(1, "npc") + unit(1, "mm")), recurse=TRUE)
unlist(unitType(min(unit(1, "in"), unit(1, "npc") + unit(1, "mm")),
                recurse=TRUE))
}
\keyword{dplot}
