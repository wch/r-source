\name{dim}
\title{Array Extents}
\usage{
dim(x)
dim(x) <- values
}
\alias{dim}
\alias{dim<-}
\value{
This function retrieves the vector of extents of the array
given as its argument.  The second form above is used to shape a
vector as an array by specifying its extents.
}
\seealso{
\code{\link{ncol}}, \code{\link{nrow}} and \code{\link{dimnames}}.
}
\examples{
# simple versions of nrow and ncol could be defined as follows
nrow0 <- function(x) dim(x)[1]
ncol0 <- function(x) dim(x)[2]
}
\keyword{array}
