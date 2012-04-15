% File src/library/datasets/man/airmiles.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{airmiles}
\docType{data}
\alias{airmiles}
\title{Passenger Miles on Commercial US Airlines, 1937--1960}
\description{
  The revenue passenger miles flown by commercial airlines in
  the United States for each year from 1937 to 1960.
}
\usage{airmiles}
\format{
  A time series of 24 observations; yearly, 1937--1960.
}
\source{F.A.A. Statistical Handbook of Aviation.}
\references{
  Brown, R. G. (1963)
  \emph{Smoothing, Forecasting and Prediction of Discrete Time Series}.
  Prentice-Hall.
}
\examples{
require(graphics)
plot(airmiles, main = "airmiles data",
     xlab = "Passenger-miles flown by U.S. commercial airlines", col = 4)
}
\keyword{datasets}
