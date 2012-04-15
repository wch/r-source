% File src/library/datasets/man/EuStockMarkets.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{EuStockMarkets}
\docType{data}
\alias{EuStockMarkets}
\title{
  Daily Closing Prices of Major European Stock Indices, 1991--1998
}
\description{
  Contains the daily closing prices of major European stock indices:
  Germany DAX (Ibis), Switzerland SMI, France CAC, and UK FTSE.  The
  data are sampled in business time, i.e., weekends and holidays are
  omitted.
}
\usage{
EuStockMarkets
}
\format{
  A multivariate time series with 1860 observations on 4 variables.
  The object is of class \code{"mts"}.
}
\source{
  The data were kindly provided by Erste Bank AG, Vienna, Austria.
}
\keyword{datasets}
