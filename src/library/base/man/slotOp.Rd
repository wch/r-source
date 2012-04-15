% File src/library/base/man/slotOp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{slotOp}
\title{Extract Slots}
\alias{@}
\description{
  Extract the contents of a slot in a object with a formal (S4) class
  structure.
}
\usage{
object@name
}
\arguments{
  \item{object}{An object from a formally defined (S4) class.}
  \item{name}{The character-string name of the slot.}
}
\details{
  This operator supports the formal classes of package \pkg{methods}, and
  is enabled only when \pkg{methods} is loaded (as per default).  See
  \code{\link{slot}} for further details.

  It is checked that \code{object} is an S4 object (see
  \code{\link{isS4}}), and it is an error to attempt to use \code{@} on
  any other object.  (There is an exception for name \code{.Data} for
  internal use only.)

  If \code{name} is not a slot name, an error is thrown. 
}
\value{
  The current contents of the slot.
}
\seealso{
  \code{\link{Extract}}, \code{\link{slot}}
}
\keyword{manip}
