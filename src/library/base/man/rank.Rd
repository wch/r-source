\name{rank}
\alias{rank}
\title{Sample Ranks}
\description{
  Returns the sample ranks of the values in a numeric vector.
  Ties, i.e., equal values, result in ranks being averaged.
}
\usage{
rank(x, na.last = TRUE)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.last}{for controlling the treatment of \code{\link{NA}}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed; if
    \code{"keep"} they are kept.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\note{
  For ranks without averaging, see the examples below.
}
\seealso{\code{\link{order}} and \code{\link{sort}}.}
\examples{
(r1 <- rank(x1 <- c(3, 1, 4, 59,26)))
(r2 <- rank(x2 <- c(3, 1, 4, 5, 9, 2, 6, 5, 3, 5))) # ties

## rank() is "idempotent": rank(rank(x)) == rank(x) :
stopifnot(rank(r1) == r1, rank(r2) == r2)

## ranks without averaging
sort.list(sort.list(x2))                 # first occurrence wins
sort.list(order(x2, runif(length(x2))))  # ties broken at random
sort.list(order(x2, runif(length(x2))))  # and again
}
\keyword{univar}
