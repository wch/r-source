\name{dimnames}
\title{Dimnames of an Object}
\alias{dimnames}
\alias{dimnames<-}
\alias{dimnames.data.frame}
\alias{dimnames<-.data.frame}
\usage{
dimnames(x)
dimnames(x) <- nlist
}
\description{
  Retrieve or set the dimnames of an object.
}
\details{
  The functions \code{dimnames} and \code{dimnames<-} are generic.

  For an array (and hence in particular, for a matrix), they retrieve or
  set the \code{dimnames} attribute of the object.

  Both have methods for data frames.  The dimnames of a data frame are
  its \code{row.names} attribute and its \code{names}.
}
\seealso{
  \code{\link{rownames}}, \code{\link{colnames}};
  \code{\link{array}}, \code{\link{matrix}}, \code{\link{data.frame}}.
}
\examples{
## simple versions of rownames and colnames
## could be defined as follows
rownames0 <- function(x) dimnames(x)[[1]]
colnames0 <- function(x) dimnames(x)[[2]]
}
\keyword{array}
\keyword{manip}
