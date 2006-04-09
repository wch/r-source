\name{is.function}
\alias{is.function}
\alias{is.primitive}
\title{Is an Object of Type (Primitive) Function?}
\usage{
is.function(x)
is.primitive(x)
}
\description{Checks whether its argument is a (primitive) function.}
\arguments{
  \item{x}{an \R object.}
}
\details{
  \code{is.function} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.

  \code{is.primitive(x)} tests if \code{x} is a primitive function
  (either a \code{"builtin"} or \code{"special"} as from \code{\link{typeof}})?
}
\value{
  \code{TRUE} if \code{x} is a (primitive) function, and \code{FALSE}
  otherwise.
}
\examples{
is.function(1) # FALSE
is.function(is.primitive)  # TRUE: it is a function, but ..
is.primitive(is.primitive) # FALSE:it's not a primitive one, whereas
is.primitive(is.function)  # TRUE: that one *is*
}
\keyword{programming}
