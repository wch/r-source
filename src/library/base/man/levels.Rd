\name{levels}
\title{Levels Attributes}
\usage{
levels(x)
levels(x) <- value
}
\alias{levels}
\alias{levels<-}
\alias{levels<-.default}
\description{
  \code{levels} provides access to the levels attribute of a variable.
  The first form returns the value of the levels of its argument
  and the second sets the attribute.

  The assignment form (\code{"levels<-"}) of \code{levels} is a generic
  function and new methods can be written for it.
  The most important method is that for \code{\link{factor}}s:
}
\seealso{\code{\link{levels<-.factor}}, \code{\link{nlevels}}.
}
\keyword{category}
