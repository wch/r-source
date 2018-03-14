% File src/library/base/man/attributes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{attributes}
\title{Object Attribute Lists}
\alias{attributes}
\alias{attributes<-}
\alias{mostattributes<-}
\description{
  These functions access an object's attributes.
  The first form below returns the object's attribute list.
  The replacement forms uses the list on the right-hand
  side of the assignment as the object's attributes (if appropriate).
}
\usage{
attributes(obj)
attributes(obj) <- value
mostattributes(obj) <- value
}
\arguments{
  \item{obj}{an object}
  \item{value}{an appropriate named list of attributes, or \code{NULL}.}
}
\details{
  Unlike \code{\link{attr}} it is possible to set attributes on a
  \code{NULL} object: it will first be coerced to an empty list.

  Note that some attributes (namely \code{\link{class}},
  \code{\link{comment}}, \code{\link{dim}}, \code{\link{dimnames}},
  \code{\link{names}}, \code{\link{row.names}} and
  \code{\link{tsp}}) are treated specially and have restrictions on
  the values which can be set.  (Note that this is not true of
  \code{\link{levels}} which should be set for factors via the
  \code{levels} replacement function.)

  Attributes are not stored internally as a list and should be thought
  of as a set and not a vector, i.e, the \emph{order} of the elements of
  \code{attributes()} does not matter.  This is also reflected by
  \code{\link{identical}()}'s behaviour with the default argument
  \code{attrib.as.set = TRUE}.  Attributes must have unique names (and
  \code{NA} is taken as \code{"NA"}, not a missing value).

  Assigning attributes first removes all attributes, then sets any
  \code{dim} attribute and then the remaining attributes in the order
  given: this ensures that setting a \code{dim} attribute always precedes
  the \code{dimnames} attribute.

  The \code{mostattributes} assignment takes special care for the
  \code{\link{dim}}, \code{\link{names}} and \code{\link{dimnames}}
  attributes, and assigns them only when known to be valid whereas an
  \code{attributes} assignment would give an error if any are not.  It
  is principally intended for arrays, and should be used with care on
  classed objects.  For example, it does not check that
  \code{\link{row.names}} are assigned correctly for data frames.

  The names of a pairlist are not stored as attributes, but are reported
  as if they were (and can be set by the replacement form of
  \code{attributes}).

  \code{\link{NULL}} objects cannot have attributes and attempts to
  assign them will promote the object to an empty list.

  Both assignment and replacement forms of \code{attributes} are
  \link{primitive} functions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{attr}},  \code{\link{structure}}.
}
\examples{
x <- cbind(a = 1:3, pi = pi) # simple matrix with dimnames
attributes(x)

## strip an object's attributes:
attributes(x) <- NULL
x # now just a vector of length 6

mostattributes(x) <- list(mycomment = "really special", dim = 3:2,
   dimnames = list(LETTERS[1:3], letters[1:5]), names = paste(1:6))
x # dim(), but not {dim}names
}
\keyword{attribute}
