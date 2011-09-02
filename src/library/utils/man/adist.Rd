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
adist(x, y = x, cost = NULL, counts = FALSE, partial = FALSE, 
      ignore.case = FALSE, useBytes = FALSE)
}
\arguments{
  \item{x}{a character vector.}
  \item{y}{a character vector.}
  \item{cost}{a numeric vector or list with names partially matching
    \samp{insertions}, \samp{deletions} and \samp{substitutions} giving
    the respective costs for computing the Levenshtein distance, or
    \code{NULL} (default) indicating using unit cost for all three
    possible transformations.  Given values are coerced to integer.}
  \item{counts}{a logical indicating whether to optionally return the
    transformation counts (numbers of insertions, deletions and
    substitutions) as the \code{"counts"} attribute of the return
    value.}
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
  and \code{y}, respectively.  If \code{counts} is \code{TRUE}, the
  transformation counts are returned as the \code{"counts"} attribute of
  this matrix, as a 3-dimensional array with dimensions corresponding to
  the elements of \code{x}, the elements of \code{y}, and the type of
  transformation (insertions, deletions and substitutions),
  respectively.
}
\details{
  The implementation is based on \samp{tre} by Ville Laurikari
  (\url{http://http://laurikari.net/tre/}).
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

## Cf. the examples for agrep:
adist("lasy", "1 lazy 2")
## For a "partial approximate match" (as used for agrep):
adist("lasy", "1 lazy 2", partial = TRUE)
}
\keyword{character}
