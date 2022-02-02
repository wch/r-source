% File src/library/base/man/slotOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{slotOp}
\title{Extract or Replace A Slot}
\alias{@}
\alias{@<-}
\description{
  Extract or replace the contents of a slot in a object with a
  formal (S4) class structure.
}
\usage{
object@name
object@name <- value
}
\arguments{
  \item{object}{An object from a formally defined (S4) class.}
  \item{name}{The character-string name of the slot, quoted or
      not.  Must be the name of a slot in the definition of the class
      of \code{object}.}
  \item{value}{A replacement value for the slot, which must be from a
      class compatible with the class defined for this slot in the
      definition of the class of  \code{object}.}
}
\details{
  These operators support the formal classes of package \pkg{methods},
  and are enabled only when package \pkg{methods} is loaded (as per
  default).  See \code{\link{slot}} for further details, in particular
  for the differences between \code{slot()} and the \code{@} operator.

  It is checked that \code{object} is an S4 object (see
  \code{\link{isS4}}), and it is an error to attempt to use \code{@} on
  any other object.  (There is an exception for name \code{.Data} for
  internal use only.)  The replacement operator checks that the slot
  already exists on the object (which it should if the object is really
  from the class it claims to be).

  These are internal generic operators: see \link{InternalMethods}.
}
\value{
  The current contents of the slot.
}
\seealso{
  \code{\link{Extract}}, \code{\link{slot}}
}
\keyword{manip}
