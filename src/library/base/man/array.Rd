% File src/library/base/man/array.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{array}
\alias{array}
\alias{as.array}
\alias{as.array.default}
\alias{is.array}
\title{Multi-way Arrays}
\description{
  Creates or tests for arrays.
}
\usage{
array(data = NA, dim = length(data), dimnames = NULL)
as.array(x, ...)
is.array(x)
}
\arguments{
  \item{data}{a vector (including a list or \code{\link{expression}}
    vector) giving data to fill the array.  Non-atomic classed objects
    are coerced by \code{\link{as.vector}}.}
  \item{dim}{the dim attribute for the array to be created, that is an
    integer vector of length one or more giving the maximal indices in
    each dimension.}
  \item{dimnames}{either \code{NULL} or the names for the dimensions.
    This must a list (or it will be ignored) with one component for each
    dimension, either \code{NULL} or a character vector of the length
    given by \code{dim} for that dimension.  The list can be named, and
    the list names will be used as names for the dimensions.  If the
    list is shorter than the number of dimensions, it is extended by
    \code{NULL}s to the length required.}
  \item{x}{an \R object.}
  \item{\dots}{additional arguments to be passed to or from methods.}
}
\details{
  An array in \R can have one, two or more dimensions.  It is simply a
  vector which is stored with additional \link{attributes} giving the
  dimensions (attribute \code{"dim"}) and optionally names for those
  dimensions (attribute \code{"dimnames"}).

  A two-dimensional array is the same thing as a \code{\link{matrix}}.

  One-dimensional arrays often look like vectors, but may be handled
  differently by some functions: \code{\link{str}} does distinguish
  them in recent versions of \R.

  The \code{"dim"} attribute is an integer vector of length one or more
  containing non-negative values: the product of the values must match
  the length of the array.

  The \code{"dimnames"} attribute is optional: if present it is a list
  with one component for each dimension, either \code{NULL} or a
  character vector of the length given by the element of the
  \code{"dim"} attribute for that dimension.

  \code{is.array} is a \link{primitive} function.

  For a list array, the \code{print} methods prints entries of length
  not one in the form \samp{integer,7} indicating the type and length.
}
\value{
  \code{array} returns an array with the extents specified in \code{dim}
  and naming information in \code{dimnames}.  The values in \code{data} are
  taken to be those in the array with the leftmost subscript moving
  fastest.  If there are too few elements in \code{data} to fill the array,
  then the elements in \code{data} are recycled.  If \code{data} has
  length zero, \code{NA} of an appropriate type is used for atomic
  vectors (\code{0} for raw vectors) and \code{NULL} for lists.

  Unlike \code{\link{matrix}}, \code{array} does not currently remove
  any attributes left by \code{as.vector} from a classed list
  \code{data}, so can return a list array with a class attribute.

  \code{as.array} is a generic function for coercing to arrays.  The
  default method does so by attaching a \code{\link{dim}} attribute to
  it.  It also attaches \code{\link{dimnames}} if \code{x} has
  \code{\link{names}}.  The sole purpose of this is to make it possible
  to access the \code{dim[names]} attribute at a later time.

  \code{is.array} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is an array (i.e., has a \code{dim} attribute of
  positive length) or not.  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\note{
  \code{is.array} is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{aperm}}, \code{\link{matrix}},
  \code{\link{dim}}, \code{\link{dimnames}}.
}
\examples{
dim(as.array(letters))
array(1:3, c(2,4)) # recycle 1:3 "2 2/3 times"
#     [,1] [,2] [,3] [,4]
#[1,]    1    3    2    1
#[2,]    2    1    3    2
}
\keyword{array}
