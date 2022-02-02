% File src/library/grid/man/getNames.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{getNames}
\alias{getNames}
\title{ List the names of grobs on the display list }
\description{
  Returns a character vector containing the names of all
  top-level grobs on the display list.
}
\usage{
getNames()
}
\value{
  A character vector.
}
\author{ Paul Murrell }
\examples{
grid.grill()
getNames()
}
\keyword{ dplot }
