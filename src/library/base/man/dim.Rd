\name{dim}
\alias{dim}
\alias{dim.data.frame}
\alias{dim<-}
\title{Dimensions of an Object}
\usage{
dim(x)
dim(x) <- values
}
\description{
  Retrieve or set the dimension of an object.
}
\arguments{
  \item{x}{an \R object, for example a matrix, array or data frame.}
}
\details{
  The functions \code{dim} and \code{dim<-} are generic.

  For an array (and hence in particular, for a matrix) they retrieve or
  set the \code{dim} attribute of the object.  It is always
  \code{\link{integer}} or \code{\link{NULL}}.

  \code{dim} has a method for \code{\link{data.frame}}s, which returns
  the length of the \code{row.names} attribute of \code{x} and the
  length of \code{x} (the numbers of ``rows'' and ``columns'').
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
