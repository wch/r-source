\name{sunspot.year}
\docType{data}
\alias{sunspot.year}
\title{
  Yearly Sunspot Data, 1700--1988
}
\description{
  Yearly numbers of sunspots. 
}
\usage{
sunspot.year
}
\format{
  The univariate time series \code{sunspot.year} contains 289
  observations, and is of class \code{"ts"}.
}
\source{
  H. Tong (1996)
  \emph{Non-Linear Time Series}. Clarendon Press, Oxford, p. 471. 
}
\note{
  Prior to \R 2.0.0 \code{sunspot.year} and \code{\link{sunspot.year}}
  were copied to the user's workspace (or elsewhere) by
  \code{data(sunspot)}. However, as package \pkg{lattice} has a dataset
  \code{sunspot} which can be retrieved by the same command, that usage
  has been removed.
}
\keyword{datasets}
