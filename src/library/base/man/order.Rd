\name{order}
\title{Ordering Permutation}
\alias{order}
\alias{sort.list}
\description{
  \code{order} returns a permutation which rearranges its first
  argument into ascending order, breaking ties by further arguments.
  \code{sort.list} is the same, using only one argument but allowing
  partial sorting.
}
\usage{
order(\dots, na.last = TRUE)
sort.list(x, partial = NULL, na.last = TRUE)
}
\arguments{
  \item{\dots}{a sequence of vectors, all of the same length.}
  \item{x}{a vector.}
  \item{partial}{vector of indices for partial sorting.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.
    }
}
\details{
  In the case of ties in the first vector, values in the second are used
  to break the ties.  If the values are still tied, values in the later
  arguments are used to break the tie (see the first example).

  \code{\link{NA}} values are treated as greater than any other values
  so that permutations returned by \code{order} move \code{NA} values to
  the top end of the array.  Other options are not (yet) implemented in
  \R.

  \code{partial} is supplied for compatibility with other
  implementations of S, but no other values are accepted and ordering is
  always complete.
}
\seealso{\code{\link{sort}} and \code{\link{rank}}.}
\examples{
(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <-c(2,1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x,y,z)[,ii] # shows the reordering (ties via 2nd & 3rd arg)

## rearrange matched vectors so that the first is in ascending order
x <- c(5:1, 6:8, 12:9)
y <- (x - 5)^2
o <- order(x)
rbind(x[o], y[o])

## tests of na.last
a <- c(4, 3, 2, NA, 1)
b <- c(4, NA, 2, 7, 1)
z <- cbind(a, b)
(o <- order(a, b)); z[o, ]
(o <- order(a, b, na.last = FALSE)); z[o, ]
(o <- order(a, b, na.last = NA)); z[o, ]
}
\keyword{univar}
\keyword{manip}
