\name{height.details}
\alias{height.details}
\title{ Height of a Grob }
\description{
  This generic function is called during the evaluation of
  \code{"grobheight"} units.  It should return an object of
  class \code{"unit"}.
}
\usage{
height.details(x)
}
\arguments{
  \item{x}{ A graphical object list structure. }
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
\seealso{
  The code for some methods, such as \code{height.details.rect}
  and \code{height.details.text}.
  The function \code{\link{absolute.size}}.
}
\keyword{dplot}
