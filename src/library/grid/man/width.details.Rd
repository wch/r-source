\name{width.details}
\alias{width.details}
\title{ Width of a Grob }
\description{
  This generic function is called during the evaluation of
  \code{"grobwidth"} units.
  It should return an object of class \code{"unit"}.
}
\usage{
width.details(x)
}
\arguments{
  \item{x}{ A graphical object list structure. }
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
\seealso{
  The code for some methods, such as \code{width.details.rect}
  and \code{width.details.text}.
  The function \code{\link{absolute.size}}.
}
\keyword{dplot}
