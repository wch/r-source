\name{class}
\alias{class}
\alias{class<-}
\title{Class of an Object}
\description{
  Returns the name of the object's class as a character string.

  In contrast to the version of this function in the base package, this
  version of \code{class} never returns \code{NULL}.  For objects that
  do not have a formal class definition, and do not have the
  \code{"class"} attribute set, the value returned is effectively the
  same as \code{data.class}.

  The replacement version of the function sets the class to the value
  provided.  For classes that have a formal definition, directly
  replacing the class this way is strongly deprecated.  The expression
  \code{as(object, value)} is the way to coerce an object to a
  particular class.
}
\usage{
class(object)

class(object) <- value
}
\arguments{
  \item{object}{
    Any R object (including basic objects for which no class is
    currently defined).  When assigning the class, however, it must be
    possible to coerce the object to the specified class:  the semantics
    of assigning a class to \code{object} are equivalent to
    \code{object <- as(object, value)}. 
  }
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\author{
  John Chambers
}
\seealso{\code{\link{as}}}
\keyword{programming}
\keyword{classes}
