% File src/library/datasets/man/islands.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{islands}
\docType{data}
\alias{islands}
\title{Areas of the World's Major Landmasses}
\description{
  The areas in thousands of square miles of the landmasses which exceed
  10,000 square miles.
}
\usage{islands}
\format{A named vector of length 48.}
\source{The World Almanac and Book of Facts, 1975, page 406.}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\examples{
require(graphics)
dotchart(log(islands, 10),
   main = "islands data: log10(area) (log10(sq. miles))")
dotchart(log(islands[order(islands)], 10),
   main = "islands data: log10(area) (log10(sq. miles))")
}
\keyword{datasets}
