\name{colnames}
\alias{colnames}
\alias{COLNAMES}
\alias{colnames<-}
\title{Column Names}
\usage{
colnames(x)
COLNAMES(x, prefix = "col")
colnames(x) <- namevector
}
\description{
  Retrieve or set the column names of an object (the second component of
  its dimnames).
  \code{COLNAMES} returns a character vector in any case, prepending
  \code{prefix} to simple column numbers, if \code{colnames(x)} is \code{NULL}.
}
\seealso{
  \code{\link{dimnames}}, \code{\link{rownames}}
}
\examples{
m0 <- matrix(NA, 4, 0)
m2 <- cbind(1,1:4)
colnames(m0)
COLNAMES(m0)
COLNAMES(m2)
}
\keyword{array}
\keyword{manip}
