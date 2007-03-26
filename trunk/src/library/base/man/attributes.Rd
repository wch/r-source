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
  Note that some attributes (namely \code{\link{class}},
  \code{\link{comment}}, \code{\link{dim}}, \code{\link{dimnames}},
  \code{\link{names}}, (from \R 2.4.0) \code{\link{row.names}} and
  \code{\link{tsp}}) are treated specially and have restrictions on
  the values which can be set.

  Attributes are not stored internally as a list and should be thought
  of as a set and not a vector.  They must have unique
  names (and \code{NA} is taken as \code{"NA"}, not a missing value).

  The \code{mostattributes} assignment takes special care for the
  \code{\link{dim}}, \code{\link{names}} and \code{\link{dimnames}}
  attributes, and assigns them only when valid whereas an
  \code{attributes} assignment would give an error if any are not.

  The names of a pairlist are not stored as attributes, but are reported
  as if they were (and can be set by the replacement method for attributes).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{attr}}.
}
\examples{
x <- cbind(a=1:3, pi=pi) # simple matrix w/ dimnames
attributes(x)

## strip an object's attributes:
attributes(x) <- NULL
x # now just a vector of length 6

mostattributes(x) <- list(mycomment = "really special", dim = 3:2,
   dimnames = list(LETTERS[1:3], letters[1:5]), names = paste(1:6))
x # dim(), but not {dim}names
}
\keyword{attribute}
