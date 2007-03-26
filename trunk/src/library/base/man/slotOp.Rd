\name{slotOp}
\title{Extract Slots}
\alias{@}
\description{
  Extract tbe contents of a slot in a object with a formal class
  structure.
}
\usage{
object@name
}
\arguments{
  \item{object}{An object from a formally defined class.}
  \item{name}{The character-string name of the slot.}
}
\details{
  This operator supports the formal classes of package \pkg{methods}, and
  is disabled unless the \pkg{methods} is loaded.
  See \code{\link[methods]{slot}} for further details.

  Currently there is no checking that the object is an instance of a
  formal class, nor that \code{name} is a slot name.
}
\seealso{
  \code{\link{Extract}},
  \code{\link[methods]{slot}}
}
\keyword{manip}
