% File src/library/base/man/array.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
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
  \item{data}{a vector (including a list) giving data to fill the array.}
  \item{dim}{the dim attribute for the array to be created, that is a
    vector of length one or more giving the maximal indices in
    each dimension.}
  \item{dimnames}{either \code{NULL} or the names for the
    dimensions. This is a list with one component for each dimension,
    either \code{NULL} or a character vector of the length given by
    \code{dim} for that dimension.  The list can be named, and the list names
    will be used as names for the dimensions.  If the list is shorter
    than the number of dimensions, it is extended by \code{NULL}s to the
    length required}
  \item{x}{an \R object.}
  \item{\dots}{additional arguments to be passed to or from methods.}
}
\value{
  \code{array} returns an array with the extents specified in \code{dim}
  and naming information in \code{dimnames}.  The values in \code{data} are
  taken to be those in the array with the leftmost subscript moving
  fastest.  If there are too few elements in \code{data} to fill the array,
  then the elements in \code{data} are recycled.  If \code{data} has
  length zero, \code{NA} of an appropriate type is used for atomic
  vectors (\code{0} for raw vectors) and \code{NULL} for lists.

  \code{as.array} is a generic function for coercing to arrays.  The
  default method does so by attaching a \code{\link{dim}} attribute to
  it.  It also attaches \code{\link{dimnames}} if \code{x} has
  \code{\link{names}}.  The sole purpose of this is to make it possible
  to access the \code{dim}[names] attribute at a later time.

  \code{is.array} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is an array (i.e., has a \code{dim} attribute of
  positive length) or not.  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\note{
  \code{is.array} is a \link{primitive} function so any argument name is
  ignored.  
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
