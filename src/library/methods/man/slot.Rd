\name{slot}
\alias{slot}
\alias{slot<-}
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
\arguments{
  \item{object}{ An object from a formally defined class.}
  \item{name}{ The character-string name of the slot. The name must be a valid
    slot name:  see Details below.}
  \item{value} { A new value for the named slot.  The value must be
    valid for this slot in this object's class.}

  \item{x}{Either the name of a class or an object from that class. Print
    \code{\link{getClass}(class)} to see the full description of the slots.} 
}
\details{
  The \code{"@"} operator and the \code{slot} function extract or
  replace the formally defined slots for the object.  The operator
  takes a fixed name, which can be unquoted if it is syntactically a
  name in the language.  A slot name can be any non-empty string, but
  if the name is not made up of letters, numbers, and \code{"."}, it
  needs to be quoted.

  In the case of the \code{slot} function, the slot name can be any
  expression that evaluates to a valid slot in the class definition.
  Generally, the only reason to use the functional form rather than
  the simpler operator is \emph{because} the slot name has to be computed.
  
  The definition of the class contains the names of all slots diretly
  and indirectly defined.  Each slot has a name and an associated
  class.  Extracting a slot returns an object from that class.  Setting
  a slot first coerces the value to the specified slot and then stores
  it.

  Unlike attributes, slots are not partially matched, and asking for (or
  trying to set) a slot with an invalid name for that class generates an
  error.
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data}, Springer.
  
  The web page \url{http://www.omegahat.org/RSMethods/index.html}
  is the primary documentation.
}
\author{
  John Chambers
}

\seealso{
  \code{\link{@}}, \code{\link{Classes}}, \code{\link{Methods}},
  \code{\link{getClass}} }

\examples{
\dontrun{
  slot(myTrack, "x")
  slot(myTrack, "y") <- log(slot(myTrack, "x"))

  slotNames("track")
}}
\keyword{programming}
\keyword{classes}

