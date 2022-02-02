% File src/library/grid/man/grobWidth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grobWidth}
\alias{grobWidth}
\alias{grobHeight}
\alias{grobAscent}
\alias{grobDescent}
\title{Create a Unit Describing the Width of a Grob}
\description{
  These functions create a unit object describing the width or height of
  a grob.  They are generic.
}
\usage{
grobWidth(x)
grobHeight(x)
grobAscent(x)
grobDescent(x)
}
\arguments{
  \item{x}{A grob object.}
}
\value{
  A unit object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit}} and
  \code{\link{stringWidth}}
}
\keyword{dplot}
