\name{slotOp}
\title{Extract or Replace Slots}
\alias{@}
\alias{@<-}
\description{
  Operators to extract or replace tbe contents of a slot in a object
  with a formal class structure.
}
\usage{
object@name
object@name <- value
}
\arguments{
  \item{object}{An object from a formally defined class.}
  \item{name}{The character-string name of the slot.}
}
\details{
  These operators support the formal classes of package \code{methods}.
  See \code{\link[methods]{slot}} for further details.
}
\seealso{
  \code{\link{Extract}}, \code{\link[methods]{slot}}
}
\keyword{manip}
