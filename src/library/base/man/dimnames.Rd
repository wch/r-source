\name{dimnames}
\title{Array Labels}
\usage{
dimnames(x)
dimnames(x) <- nlist
}
\alias{dimnames}
\alias{dimnames<-}
\alias{dimnames.data.frame}
\alias{dimnames<-.data.frame}
\value{
  This generic function obtains the list of extent labels of the array given as
  its argument.  The second form above can be used to set these labels.
}
\seealso{
  \code{\link{matrix}}, \code{\link{rownames}}, \code{\link{colnames}}.
}
\examples{
## simple versions of rownames and colnames
## could be defined as follows
rownames0 <- function(x) dimnames(x)[[1]]
colnames0 <- function(x) dimnames(x)[[2]]
}
\keyword{array}
\keyword{manip}
