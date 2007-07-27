% File src/library/grid/man/stringWidth.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{stringWidth}
\alias{stringWidth}
\alias{stringHeight}
\title{Create a Unit Describing the Width of a String}
\description{
  These functions create a unit object describing the width or height of
  a string.
}
\usage{
stringWidth(string)
stringHeight(string)
}
\arguments{
  \item{string}{A character vector.}
}
\value{
  A unit object. 
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}} and
  \code{\link{grobWidth}}
}
\keyword{dplot}
