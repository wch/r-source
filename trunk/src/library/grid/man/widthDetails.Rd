\name{widthDetails}
\alias{widthDetails}
\alias{heightDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Width and Height of a grid grob }
\description{
  These generic functions are used to determine the size of grid grobs.
}
\usage{
widthDetails(x)
heightDetails(x)
}
%- maybe also 'usage' for other objects documented here.
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
\keyword{ dplot }% at least one, from doc/KEYWORDS
