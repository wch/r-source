\name{dim}
\title{Array Extents}
\usage{
dim(x)
dim(x) <- values
}
\alias{dim}
\alias{dim.data.frame}
\alias{dim<-}
\value{
These generic functions retrieve the vector of extents of the array
\code{x}.  The second form above is used to shape a
nvector \code{x} as an array by specifying its extents \code{values}.
}
\seealso{
\code{\link{ncol}}, \code{\link{nrow}} and \code{\link{dimnames}}.
}
\examples{
x <- 1:12 ; dim(x) <- c(3,4)
x

# simple versions of nrow and ncol could be defined as follows
nrow0 <- function(x) dim(x)[1]
ncol0 <- function(x) dim(x)[2]
}
\keyword{array}
