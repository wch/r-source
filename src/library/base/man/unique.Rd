% File src/library/base/man/unique.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{unique}
\alias{unique}
\alias{unique.default}
\alias{unique.data.frame}
\alias{unique.matrix}
\alias{unique.array}
\title{Extract Unique Elements}
\description{
  \code{unique} returns a vector, data frame or array like \code{x}
  but with duplicate elements/rows removed.
}
\usage{
unique(x, incomparables = FALSE, \dots)

\method{unique}{default}(x, incomparables = FALSE, fromLast = FALSE,
        nmax = NA, \dots)

\method{unique}{matrix}(x, incomparables = FALSE, MARGIN = 1,
       fromLast = FALSE, \dots)

\method{unique}{array}(x, incomparables = FALSE, MARGIN = 1,
       fromLast = FALSE, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{a vector of values that cannot be compared.
    \code{FALSE} is a special value, meaning that all values can be
    compared, and may be the only value accepted for methods other than
    the default.  It will be coerced internally to the same type as
       \code{x}.}
  \item{fromLast}{logical indicating if duplication should be considered
    from the last, i.e., the last (or rightmost) of identical elements will
    be kept.  This only matters for \code{\link{names}} or
    \code{\link{dimnames}}.}
  \item{nmax}{the maximum number of unique items expected (greater than one).
    See \code{\link{duplicated}}.}
  \item{\dots}{arguments for particular methods.}
  \item{MARGIN}{the array margin to be held fixed: a single integer.}
}
\details{
  This is a generic function with methods for vectors, data frames and
  arrays (including matrices).

  The array method calculates for each element of the dimension
  specified by \code{MARGIN} if the remaining dimensions are identical
  to those for an earlier element (in row-major order).  This would most
  commonly be used for matrices to find unique rows (the default) or columns
  (with \code{MARGIN = 2}).

  Note that unlike the Unix command \code{uniq} this omits
  \emph{duplicated} and not just \emph{repeated} elements/rows.  That
  is, an element is omitted if it is equal to any previous element and
  not just if it is equal the immediately previous one.  (For the
  latter, see \code{\link{rle}}).

  Missing values (\code{"\link{NA}"}) are regarded as equal, numeric and
  complex ones differing from \code{NaN}; character strings will be compared in a
  \dQuote{common encoding}; for details, see \code{\link{match}} (and
  \code{\link{duplicated}}) which use the same concept.

  Values in \code{incomparables} will never be marked as duplicated.
  This is intended to be used for a fairly small set of values and will
  not be efficient for a very large set.

  When used on a data frame with more than one column, or an array or
  matrix when comparing dimensions of length greater than one, this
  tests for identity of character representations.  This will
  catch people who unwisely rely on exact equality of floating-point
  numbers!
}
\value{
  For a vector, an object of the same type of \code{x}, but with only
  one copy of each duplicated element.  No attributes are copied (so
  the result has no names).

  For a data frame, a data frame is returned with the same columns but
  possibly fewer rows (and with row names from the first occurrences of
  the unique rows).

  A matrix or array is subsetted by \code{[, drop = FALSE]}, so
  dimensions and dimnames are copied appropriately, and the result
  always has the same number of dimensions as \code{x}.
}
\section{Warning}{
  Using this for lists is potentially slow, especially if the elements
  are not atomic vectors (see \code{\link{vector}}) or differ only
  in their attributes.  In the worst case it is \eqn{O(n^2)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{duplicated}} which gives the indices of duplicated
  elements.

  \code{\link{rle}} which is the equivalent of the Unix \code{uniq -c}
  command.
}
\examples{
x <- c(3:5, 11:8, 8 + 0:5)
(ux <- unique(x))
(u2 <- unique(x, fromLast = TRUE)) # different order
stopifnot(identical(sort(ux), sort(u2)))

length(unique(sample(100, 100, replace = TRUE)))
## approximately 100(1 - 1/e) = 63.21

unique(iris)
}
\keyword{manip}
\keyword{logic}
