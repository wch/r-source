% File src/library/datasets/man/precip.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{precip}
\docType{data}
\alias{precip}
\title{Annual Precipitation in US Cities}
\description{
  The average amount of precipitation (rainfall) in inches for each of
  70 United States (and Puerto Rico) cities.
}
\usage{precip}
\format{
  A named vector of length 70.
}
\source{
  Statistical Abstracts of the United States, 1975.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(graphics)
dotchart(precip[order(precip)], main = "precip data")
title(sub = "Average annual precipitation (in.)")
}
\keyword{datasets}
