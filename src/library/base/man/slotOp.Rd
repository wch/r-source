% File src/library/base/man/slotOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{slotOp}
\title{Extract or Replace a Slot or Property}
\alias{@}
\alias{@<-}
\description{
  Extract or replace the contents of a slot or property of an object.
}
\usage{
object@name
object@name <- value
}
\arguments{
  \item{object}{An object from a formally defined (S4) class, or an
    object with a class for which `@` or `@<-` S3 methods are defined.}
  \item{name}{The name of the slot or property, supplied as a character
    string or unquoted symbol. If \code{object} has an S4 class, then
    \code{name} must be the name of a slot in the definition of the class
    of \code{object}.}
  \item{value}{A suitable replacement value for the slot or
    property. For an S4 object this must be from a class compatible
    with the class defined for this slot in the definition of the class
    of \code{object}.}
}
\details{
  If \code{object} is not an S4 object, then a suitable S3 method for
  `@` or `@<-` is searched for. If no method is found, then an error
  is signaled.

  if \code{object} is an S4 object, then these operators are for slot
  access, and are enabled only when package \pkg{methods} is loaded (as
  per default).  The slot must be formally defined. (There is an
  exception for the name \code{.Data}, intended for internal use only.)
  The replacement operator checks that the slot already exists on the
  object (which it should if the object is really from the class it
  claims to be).  See \code{\link{slot}} for further details, in
  particular for the differences between \code{slot()} and the \code{@}
  operator.

  These are internal generic operators: see \link{InternalMethods}.
}
\value{
  The current contents of the slot.
}
\seealso{
  \code{\link{Extract}}, \code{\link{slot}}
}
\keyword{manip}
