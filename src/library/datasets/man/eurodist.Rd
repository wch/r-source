% File src/library/datasets/man/eurodist.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{eurodist}
\docType{data}
\alias{eurodist}
\alias{UScitiesD}
\title{Distances Between European Cities and Between US Cities}
\description{
  The \code{eurodist} gives the road distances (in km) between 21
  cities in Europe.  The data are taken from a table in \emph{The
  Cambridge Encyclopaedia}.

  \code{UScitiesD} gives \dQuote{straight line} distances between 10
  cities in the US.
}
\usage{
eurodist
UScitiesD
}
\format{
  \code{dist} objects based on 21 and 10 objects, respectively.
  (You must have the \pkg{stats} package loaded to have the methods for this
  kind of object available).
}
\source{
  \bibshow{R:Crystal:1990}

  The US cities distances were provided by Pierre Legendre.
}
\keyword{datasets}
