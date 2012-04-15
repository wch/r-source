% File src/library/datasets/man/discoveries.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{discoveries}
\docType{data}
\alias{discoveries}
\title{Yearly Numbers of Important Discoveries}
\description{
  The numbers of \dQuote{great} inventions and scientific
  discoveries in each year from 1860 to 1959.
}
\usage{discoveries}
\format{A time series of 100 values.}
\source{
  The World Almanac and Book of Facts, 1975 Edition, pages 315--318.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\examples{
require(graphics)
plot(discoveries, ylab = "Number of important discoveries",
     las = 1)
title(main = "discoveries data set")
}
\keyword{datasets}
