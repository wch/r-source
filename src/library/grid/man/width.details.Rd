\name{width.details}
\alias{width.details}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Width of a Grob }
\description{
  This generic function is called during the evaluation of
  \code{"grobwidth"} units.  It should return an object of
  class {\tt "unit"}.
}
\usage{
width.details(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ A graphical object list structure. }
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
\seealso{
  The code for some methods, such as \code{width.details.rect}
  and \code{width.details.text}.  The function \code{\link{absolute.size}}.
}
\keyword{ dplot }% at least one, from doc/KEYWORDS

