% File src/library/datasets/man/precip.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
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
\note{
  The dataset version up to Nov.16, 2016 had a typo in \code{"Cincinnati"}'s
  name.  The examples show how to recreate that version.
}
\examples{
require(graphics)
dotchart(precip[order(precip)], main = "precip data")
title(sub = "Average annual precipitation (in.)")

## Old ("wrong") version of dataset (just name change):
precip.O <- local({
   p <- precip; names(p)[names(p) == "Cincinnati"] <- "Cincinati" ; p })
stopifnot(all(precip == precip.O),
	  match("Cincinnati", names(precip)) == 46,
	  identical(names(precip)[-46], names(precip.O)[-46]))
}
\keyword{datasets}
