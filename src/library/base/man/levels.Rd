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
\arguments{
  \item{x}{an object, for example a factor.}
  \item{value}{A valid value for \code{levels(x)}.
    For the default method, \code{NULL} or a character vector.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{levels<-.factor}}, \code{\link{nlevels}}.
}
\keyword{category}
