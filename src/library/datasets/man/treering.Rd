% File src/library/datasets/man/treering.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{treering}
\docType{data}
\alias{treering}
\title{
  Yearly Treering Data, -6000--1979
}
\description{
  Contains normalized tree-ring widths in dimensionless units.
}
\usage{
treering
}
\format{
  A univariate time series with 7981 observations. The object is of
  class \code{"ts"}.

  Each tree ring corresponds to one year.
}
\details{
  The data were recorded by Donald A. Graybill, 1980, from
  Gt Basin Bristlecone Pine 2805M, 3726-11810 in Methuselah Walk, California.
}
\source{
  Time Series Data Library:
  \url{http://www-personal.buseco.monash.edu.au/~hyndman/TSDL/},
  series \file{CA535.DAT}
  %% now pointing to  \url{http://datamarket.com/data/list/?q=provider:tsdl}
  %% or specifially to
  %% \url{https://datamarket.com/data/set/22oy/} ==> short \url{http://data.is/2iSo2j1}
}
\references{
  For some photos of Methuselah Walk see
  \url{https://web.archive.org/web/20110523225828/http://www.ltrr.arizona.edu/~hallman/sitephotos/meth.html}
}
\keyword{datasets}
