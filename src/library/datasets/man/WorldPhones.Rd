% File src/library/datasets/man/WorldPhones.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{WorldPhones}
\docType{data}
\alias{WorldPhones}
\title{The World's Telephones}
\description{
  The number of telephones in various regions of the world (in
  thousands).
}
\usage{WorldPhones}
\format{
  A matrix with 7 rows and 8 columns.  The columns of the matrix give
  the figures for a given region, and the rows the figures for a year.

  The regions are: North America, Europe, Asia, South America, Oceania,
  Africa, Central America.

  The years are: 1951, 1956, 1957, 1958, 1959, 1960, 1961.
}
\source{
  AT&T (1961) \emph{The World's Telephones}.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(graphics)
matplot(rownames(WorldPhones), WorldPhones, type = "b", log = "y",
        xlab = "Year", ylab = "Number of telephones (1000's)")
legend(1951.5, 80000, colnames(WorldPhones), col = 1:6, lty = 1:5,
       pch = rep(21, 7))
title(main = "World phones data: log scale for response")
}
\keyword{datasets}
