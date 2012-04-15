% File src/library/grid/man/unit.length.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unit.length}
\alias{unit.length}
\title{Length of a Unit Object}
\description{
  The length of a unit object is defined as the number of unit
  values in the unit object.

  This function has been deprecated in favour of a unit method for
  the generic \code{length} function.
}
\usage{
unit.length(unit)
}
\arguments{
  \item{unit}{A unit object.}
}
\value{
  An integer value.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}}
}
\examples{
length(unit(1:3, "npc"))
length(unit(1:3, "npc") + unit(1, "inches"))
length(max(unit(1:3, "npc") + unit(1, "inches")))
length(max(unit(1:3, "npc") + unit(1, "strwidth", "a"))*4)
length(unit(1:3, "npc") + unit(1, "strwidth", "a")*4)
}
\keyword{dplot}
