% File src/library/base/man/bincode.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{.bincode}
\alias{.bincode}
\title{Bin a Numeric Vector}
\description{
  Bin a numeric vector and return integer codes for the binning.
}
\usage{
.bincode(x, breaks, right = TRUE, include.lowest = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector which is to be converted to a factor by cutting.}
  \item{breaks}{either a numeric vector of two or more cut points.}
  \item{include.lowest}{logical, indicating if an \sQuote{x[i]} equal to
    the lowest (or highest, for \code{right = FALSE}) \sQuote{breaks}
    value should be included.}
  \item{right}{logical, indicating if the intervals should be closed on
    the right (and open on the left) or vice versa.}
}
\value{
  An integer vector of the same length as \code{x} indicating which bin
  each element falls into (the leftmost bin being bin \code{1}).
}
\seealso{
  \code{\link{cut}}, \code{\link{tabulate}}
}
\keyword{category}
