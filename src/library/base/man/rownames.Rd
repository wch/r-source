\name{rownames}
\alias{rownames}
\alias{ROWNAMES}
\alias{rownames<-}
\title{Row Names}
\usage{
rownames(x)
ROWNAMES(x, prefix = "row")
rownames(x) <- namevector
}
\description{
  Retrieve or set the row names of an object (the first component of its
  dimnames).
  \code{ROWNAMES} returns a character vector in any case, prepending
  \code{prefix} to simple row numbers, if \code{rownames(x)} is \code{NULL}.
}
\seealso{
  \code{\link{dimnames}}, \code{\link{colnames}}
}
\keyword{array}
\keyword{manip}
