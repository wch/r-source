% File src/library/base/man/rank.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{rank}
\alias{rank}
\title{Sample Ranks}
\description{
  Returns the sample ranks of the values in a vector.  Ties (i.e., equal
  values) and missing values can be handled in several ways.
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
    \code{"keep"} they are kept with rank \code{NA}.}
  \item{ties.method}{a character string specifying how ties are treated,
    see \sQuote{Details}; can be abbreviated.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\details{
  If all components are different (and no \code{NA}s), the ranks are
  well defined, with values in \code{seq_len(x)}.  With some values equal
  (called \sQuote{ties}), the argument \code{ties.method} determines the
  result at the corresponding indices.  The \code{"first"} method results
  in a permutation with increasing values at each index set of ties.
  The \code{"random"} method puts these in random order whereas the
  default, \code{"average"}, replaces them by their mean, and
  \code{"max"} and \code{"min"} replaces them by their maximum and
  minimum respectively, the latter being the typical sports
  ranking.

  \code{NA} values are never considered to be equal: for \code{na.last =
    TRUE} and \code{na.last = FALSE} they are given distinct ranks in
  the order in which they occur in \code{x}.

  \strong{NB}: \code{rank} is not itself generic but \code{\link{xtfrm}}
  is, and \code{rank(xtfrm(x), ....)} will have the desired result if
  there is a \code{xtfrm} method.  Otherwise, \code{rank} will make use
  of \code{==}, \code{>}, \code{is.na} and extraction methods for
  classed objects, possibly rather slowly.
}
\value{
  A numeric vector of the same length as \code{x} with names copied from
  \code{x} (unless \code{na.last = NA}, when missing values are
  removed).  The vector is of integer type unless \code{x} is a long
  vector or \code{ties.method = "average"} when it is of double type
  (whether or not there are any ties).
}
\seealso{
  \code{\link{order}} and \code{\link{sort}}.
}
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
