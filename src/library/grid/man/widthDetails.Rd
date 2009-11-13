% File src/library/grid/man/widthDetails.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{widthDetails}
\alias{widthDetails}
\alias{heightDetails}
\title{ Width and Height of a grid grob }
\description{
  These generic functions are used to determine the size of grid grobs.
}
\usage{
widthDetails(x)
heightDetails(x)
}
\arguments{
  \item{x}{ A grid grob. }
}
\details{
  These functions are called in the calculation of
  \code{"grobwidth"} and \code{"grobheight"} units.
  Methods should be written for classes derived from grob or gTree
  where the size of the grob can be determined (see, for example
  \code{grid:::widthDetails.frame}).
}
\value{
  A unit object.
}
\seealso{
  \code{\link{absolute.size}}.
}
\author{ Paul Murrell }
\keyword{ dplot }
