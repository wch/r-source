% File src/library/utils/man/adist.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011-2015 R Core Team
% Distributed under GPL 2 or later

\name{adist}
\alias{adist}
\title{Approximate String Distances}
\description{
  Compute the approximate string distance between character vectors.
  The distance is a generalized Levenshtein (edit) distance, giving the
  minimal possibly weighted number of insertions, deletions and
  substitutions needed to transform one string into another.
}
\usage{
adist(x, y = NULL, costs = NULL, counts = FALSE, fixed = TRUE,
      partial = !fixed, ignore.case = FALSE, useBytes = FALSE)
}
\arguments{
  \item{x}{a character vector.  \link{Long vectors} are not supported.}
  \item{y}{a character vector, or \code{NULL} (default) indicating
    taking \code{x} as \code{y}.}
  \item{costs}{a numeric vector or list with names partially matching
    \samp{insertions}, \samp{deletions} and \samp{substitutions} giving
    the respective costs for computing the Levenshtein distance, or
    \code{NULL} (default) indicating using unit cost for all three
    possible transformations.}
  \item{counts}{a logical indicating whether to optionally return the
    transformation counts (numbers of insertions, deletions and
    substitutions) as the \code{"counts"} attribute of the return
    value.}
  \item{fixed}{a logical.  If \code{TRUE} (default), the \code{x}
    elements are used as string literals.  Otherwise, they are taken as
    regular expressions and \code{partial = TRUE} is implied
    (corresponding to the approximate string distance used by
    \code{\link{agrep}} with \code{fixed = FALSE}.}
  \item{partial}{a logical indicating whether the transformed \code{x}
    elements must exactly match the complete \code{y} elements, or only
    substrings of these.  The latter corresponds to the approximate
    string distance used by \code{\link{agrep}} (by default).}
  \item{ignore.case}{a logical.  If \code{TRUE}, case is ignored for
    computing the distances.}
  \item{useBytes}{a logical.  If \code{TRUE} distance computations are
    done byte-by-byte rather than character-by-character.}
}
\value{
  A matrix with the approximate string distances of the elements of
  \code{x} and \code{y}, with rows and columns corresponding to \code{x}
  and \code{y}, respectively.

  If \code{counts} is \code{TRUE}, the transformation counts are
  returned as the \code{"counts"} attribute of this matrix, as a
  3-dimensional array with dimensions corresponding to the elements of
  \code{x}, the elements of \code{y}, and the type of transformation
  (insertions, deletions and substitutions), respectively.
  Additionally, if \code{partial = FALSE}, the transformation sequences
  are returned as the \code{"trafos"} attribute of the return value, as
  character strings with elements \samp{M}, \samp{I}, \samp{D} and
  \samp{S} indicating a match, insertion, deletion and substitution,
  respectively.  If \code{partial = TRUE}, the offsets (positions of
  the first and last element) of the matched substrings are returned as
  the \code{"offsets"} attribute of the return value (with both offsets
  \eqn{-1} in case of no match).
}
\details{
  The (generalized) Levenshtein (or edit) distance between two strings
  \var{s} and \var{t} is the minimal possibly weighted number of
  insertions, deletions and substitutions needed to transform \var{s}
  into \var{t} (so that the transformation exactly matches \var{t}).
  This distance is computed for \code{partial = FALSE}, currently using
  a dynamic programming algorithm (see, e.g.,
  \url{http://en.wikipedia.org/wiki/Levenshtein_distance}) with space
  and time complexity \eqn{O(mn)}, where \eqn{m} and \eqn{n} are the
  lengths of \var{s} and \var{t}, respectively.  Additionally computing
  the transformation sequence and counts is \eqn{O(\max(m, n))}.

  The generalized Levenshtein distance can also be used for approximate
  (fuzzy) string matching, in which case one finds the substring of
  \var{t} with minimal distance to the pattern \var{s} (which could be
  taken as a regular expression, in which case the principle of using
  the leftmost and longest match applies), see, e.g.,
  \url{http://en.wikipedia.org/wiki/Approximate_string_matching}.  This
  distance is computed for \code{partial = TRUE} using \samp{tre} by
  Ville Laurikari (\url{http://laurikari.net/tre/}) and
  corresponds to the distance used by \code{\link{agrep}}.  In this
  case, the given cost values are coerced to integer.

  Note that the costs for insertions and deletions can be different, in
  which case the distance between \var{s} and \var{t} can be different
  from the distance between \var{t} and \var{s}.
}
\seealso{
  \code{\link{agrep}} for approximate string matching (fuzzy matching)
  using the generalized Levenshtein distance.
}
\examples{
## Cf. http://en.wikipedia.org/wiki/Levenshtein_distance
adist("kitten", "sitting")
## To see the transformation counts for the Levenshtein distance:
drop(attr(adist("kitten", "sitting", counts = TRUE), "counts"))
## To see the transformation sequences:
attr(adist(c("kitten", "sitting"), counts = TRUE), "trafos")

## Cf. the examples for agrep:
adist("lasy", "1 lazy 2")
## For a "partial approximate match" (as used for agrep):
adist("lasy", "1 lazy 2", partial = TRUE)
}
\keyword{character}
