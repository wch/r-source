\name{height.details}
\alias{height.details}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Height of a Grob }
\description{
  This generic function is called during the evaluation of
  \code{"grobheight"} units.  It should return an object of
  class {\tt "unit"}.
}
\usage{
height.details(x)
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
  The code for some methods, such as \code{height.details.rect}
  and \code{height.details.text}.  The function \code{\link{absolute.size}}.
}
\keyword{ dplot }% at least one, from doc/KEYWORDS

