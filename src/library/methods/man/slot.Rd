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
slot(object, name, check = TRUE) <- value

slotNames(x)
}
\arguments{
  \item{object}{ An object from a formally defined class.}
  \item{name}{ The character-string name of the slot. The name must be a
    valid slot name:  see Details below.}
  \item{value}{ A new value for the named slot.  The value must be
    valid for this slot in this object's class.}
  \item{x}{Either the name of a class or an object from that class.
    Print \code{\link{getClass}(class)} to see the full description of
    the slots.}
  \item{check}{If \code{TRUE}, check the assigned value for validity
    as the value of this slot.  You should never set this to
    \code{FALSE} in normal use, since the result can create invalid objects.
    }

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

  The definition of the class contains the names of all slots directly
  and indirectly defined.  Each slot has a name and an associated
  class.  Extracting a slot returns an object from that class.  Setting
  a slot first coerces the value to the specified slot and then stores
  it.

  Unlike attributes, slots are not partially matched, and asking for (or
  trying to set) a slot with an invalid name for that class generates an
  error.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\seealso{
  \code{\link{@}},
  \code{\link{Classes}},
  \code{\link{Methods}},
  \code{\link{getClass}}
}
\examples{
\dontrun{
slot(myTrack, "x")
slot(myTrack, "y") <- log(slot(myTrack, "x"))

slotNames("track")
}}
\keyword{programming}
\keyword{classes}
