\name{rank}
\alias{rank}
\title{Sample Ranks}
\description{
  Returns the sample ranks of the values in a numeric vector.
  Ties result in ranks being averaged.
}
\usage{
rank(x, na.last = TRUE)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
}
\seealso{\code{\link{order}} and \code{\link{sort}}.}
\examples{
(r1 <- rank(x1 <- c(3,1,4,59,26)))
(r2 <- rank(x2 <- c(3,1,4,5,9,2,6,5,3,5))) # ties

## rank() is "idempotent": rank(rank(x)) == rank(x) :
stopifnot(rank(r1) == r1, rank(r2) == r2)
}
\keyword{univar}
