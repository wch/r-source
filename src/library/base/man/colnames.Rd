\name{row/colnames}
\title{Row and Columnn Names}
\alias{rownames}
\alias{rownames<-}
\alias{colnames}
\alias{colnames<-}
\usage{
rownames(x, do.NULL = TRUE, prefix = "row")
rownames(x) <- namevector

colnames(x, do.NULL = TRUE, prefix = "col")
colnames(x) <- namevector
}
\description{
  Retrieve or set the row or column names of an object (the first or
  second component of its \code{\link{dimnames}}).
}
\details{
  If \code{do.NULL} is \code{FALSE}, a character vector (of length
  \code{\link{NROW}(x)} or \code{\link{NCOL}(x)} is returned in any
  case, prepending \code{prefix} to simple numbers, if
  \code{dimnames(x)[[i]]} (\code{i =} 1 or 2) is \code{NULL}. 
}
\seealso{\code{\link{dimnames}}, \code{\link{case.names}},
    \code{\link{variable.names}}.
}
\examples{
m0 <- matrix(NA, 4, 0)
m2 <- cbind(1,1:4)
rownames(m0)
\testonly{
        rownames(m0, do.NULL = FALSE)
        colnames(m0, do.NULL = FALSE)
}
colnames(m2, do.NULL = FALSE)
colnames(m2) <- c("x","Y")
rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")
m2
}
\keyword{array}
\keyword{manip}
