\name{regmatches}
\alias{regmatches}
\title{Extract Matched Substrings}
\description{
  Extract matched substrings from match data obtained by
  \code{\link{regexpr}} or \code{\link{gregexpr}}.
}
\usage{
regmatches(x, m, invert = FALSE)
}
\arguments{
  \item{x}{a character vector}
  \item{m}{an object with match data}
  \item{invert}{a logical: if \code{TRUE}, extract the non-matched
    substrings.}
}
\details{
  If \code{invert} is \code{TRUE} (default), the matched substrings as
  specified by the match data are extracted.  For vector match data,
  empty matches are dropped; for list match data, empty matches give
  empty components (zero-length character vectors).
  
  If \code{invert} is \code{FALSE}, the non-matched substrings are
  extracted, i.e., the strings are split according to the matches
  similar to \code{\link{strsplit}} (for vector match data, at most a
  single split is performed).

  Note that the match data can be obtained from regular expression
  matching on a modified version of \code{x} with the same numbers of
  characters.
}
\value{
  For vector match data (as obtained from \code{\link{regexpr}}), a
  character vector with the matched substrings provided that
  \code{invert} is \code{FALSE}.  Otherwise, a list with the matched or
  non-matched substrings.
}
\examples{
x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
pattern <- "[[:space:]]*(,|and)[[:space:]]"
## Match data from regexpr()
m <- regexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)
## Match data from gregexpr()
m <- gregexpr(pattern, x)
regmatches(x, m)
regmatches(x, m, invert = TRUE)
}
\keyword{character}
\keyword{utilities}

