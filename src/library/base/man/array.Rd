\name{array}
\alias{array}
\alias{as.array}
\alias{is.array}
\title{Multi-way Arrays}
\description{
  Creates or tests for arrays.
}
\usage{
array(x, dim = length(x), dimnames = NULL)
as.array(x)
is.array(x)
}
\value{
  \code{array} returns an array with the extents specified in \code{dim}
  and naming information in \code{dimnames}.  The values in \code{x} are
  taken to be those in the array with the leftmost subscript moving
  fastest.  If there are too few elements in \code{x} to fill the array,
  then the elements in \code{x} are recycled.

  \code{as.array()} coerces its argument to be an array by attaching a
  \code{\link{dim}} attribute to it.  It also attaches
  \code{\link{dimnames}} if \code{x} has \code{\link{names}}.
  The sole purpose of this is to make it possible to access the
  \code{dim}[names] attribute at a later time.

  \code{is.array} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is an array (i.e., has a \code{dim} attribute) 
  or not.
}
\seealso{
  \code{\link{aperm}},
  \code{\link{matrix}}.
}
\examples{
dim(as.array(letters))
array(1:3, c(2,4)) # recycle 1:3 "2 2/3 times"
#     [,1] [,2] [,3] [,4]
#[1,]    1    3    2    1
#[2,]    2    1    3    2
}
\keyword{array}
