\name{rank}
\alias{rank}
\title{Sample Ranks}
\description{
  Returns the sample ranks of the values in a vector.
  Ties, i.e., equal values, result in ranks being averaged, by default.
}
\usage{
rank(x, na.last = TRUE,
     ties.method = c("average", "first", "random", "max", "min"))
}
\arguments{
  \item{x}{a numeric, complex, character or logical vector.}
  \item{na.last}{for controlling the treatment of \code{\link{NA}}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed; if
    \code{"keep"} they are kept.}
  \item{ties.method}{a character string specifying how ties are treated,
    see below; can be abbreviated.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\details{
  If all components are different, the ranks are well defined, with
  values in \code{1:n} where \code{n <- length(x)} and we assume no
  \code{NA}s for the moment.  Otherwise, with some values equal, called
  \sQuote{ties}, the argument \code{ties.method} determines
  the result at the corresponding indices.  The \code{"first"} method
  results in a permutation with increasing values at each index set of
  ties.  The \code{"random"} method puts these in random order whereas the
  default, \code{"average"}, replaces them by their mean, and
  \code{"max"} and \code{"min"} replaces them by their maximum and
  minimum respectively, the latter being the typical \dQuote{sports}
  ranking.
}
\seealso{\code{\link{order}} and \code{\link{sort}}.}
\examples{
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged

## rank() is "idempotent": rank(rank(x)) == rank(x) :
stopifnot(rank(r1) == r1, rank(r2) == r2)

## ranks without averaging
rank(x2, ties.method= "first")  # first occurrence wins
rank(x2, ties.method= "random") # ties broken at random
rank(x2, ties.method= "random") # and again

## keep ties ties, no average
(rma <- rank(x2, ties.method= "max"))  # as used classically
(rmi <- rank(x2, ties.method= "min"))  # as in Sports
stopifnot(rma + rmi == round(r2 + r2))
}
\keyword{univar}
