\name{slot}
\alias{slot}
\alias{slot<-}
\alias{@}
\alias{@<-}
\alias{slotNames}
\title{ The Slots in an Object from a Formal Class }
\description{
  These functions return or set information about the individual slots
  in an object.
}
\usage{
object@name
object@name <- value


slot(object, name)
slot(object, name) <- value

slotNames(x)

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{object}{ An object from a formally defined class }
  \item{name}{ The character-string name of the slot.  In the case of
    the \code{@} operator, this name is a literal, and is \emph{not}
    evaluated. If you need to compute the slot name, use the function
    \code{slot}, rather than the \code{@} opertor.  The name must be a valid
    slot name:  see Details below. }
  \item{value} { A new value for the named slot.  The value must be
    valid for this slot in this object's class.}

  \item{x}{Either the name of a class or an object from that class. Print
    \code{\link{getClass}(class)} to see the full description of the slots.} 
}
\details{
  The definition of the class contains the names of all slots diretly
  and indirectly defined.  Each slot has a name and an associated
  class.  Extracting a slot returns an object from that class.  Setting
  a slot first coerces the value to the specified slot and then stores
  it.

  Unlike attributes, slots are not partially matched, and asking for (or
  trying to set) a slot with an invalid name for that class generates an
  error).
  
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}

\seealso{ \link{Classes}, \link{Methods}, \code{\link{getClass}} }

\examples{
\dontrun{
  slot(myTrack, "x")
  slot(myTrack, "y") <- log(slot(myTrack, "x"))

  slotNames("track")
}

}
\keyword{programming}
\keyword{classes}

