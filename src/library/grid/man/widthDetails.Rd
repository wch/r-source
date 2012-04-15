% File src/library/grid/man/widthDetails.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{widthDetails}
\alias{widthDetails}
\alias{heightDetails}
\alias{ascentDetails}
\alias{descentDetails}
\title{ Width and Height of a grid grob }
\description{
  These generic functions are used to determine the size of grid grobs.
}
\usage{
widthDetails(x)
heightDetails(x)
ascentDetails(x)
descentDetails(x)
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

  The ascent of a grob is the height of the grob by default and
  the descent of a grob is zero by default, except for text grobs
  where the label is a single character value or expression.
}
\value{
  A unit object.
}
\seealso{
  \code{\link{absolute.size}}.
}
\author{ Paul Murrell }
\keyword{ dplot }
