\name{rank}
\title{Sample Ranks}
\usage{
rank(x)
}
\alias{rank}
\description{
  \code{rank} returns the sample ranks of the values in \code{x}.
  Ties result in ranks being averaged.
  \code{\link{NA}} values are not allowed in this function.
}
\seealso{\code{\link{order}} and \code{\link{sort}}.}
\examples{
(r1 <- rank(x1 <- c(3,1,4,59,26)))
(r2 <- rank(x2 <- c(3,1,4,5,9,2,6,5,3,5))) # ties

## rank() is "idempotent": rank(rank(x)) == rank(x) :
stopifnot(rank(r1) == r1, rank(r2) == r2)
}
\keyword{univar}
