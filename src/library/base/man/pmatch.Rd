\name{pmatch}
\title{Partial String Matching}
\usage{
pmatch(x, table, nomatch = NA, duplicates.ok = FALSE)
}
\alias{pmatch}
\arguments{
  \item{x}{the values to be matched.}
  \item{table}{the values to be matched against.}
  \item{nomatch}{the value returned at non-matching or mutliply
    partially matching positions.}
  \item{duplicates.ok}{should elements be in \code{table} be used more
    than once?}
}
\description{
  \code{pmatch} seeks matches for the elements of its first argument
  among those of its second.
}
\details{
  The behaviour differs by the value of \code{duplicates.ok}. Consider
  first the case if this is true.  First exact matches are considered,
  and the positions of the first exact matches are recorded. Then unique
  partial matches are considered, and if found recorded.  Finally,
  all remaining elements of \code{x} are regarded as unmatched.
  In addition, an empty string can match nothing, not even an exact
  match to an empty string.  This is the appropriate behaviour for
  partial matching of character indices, for example

  If \code{duplicates.ok} is \code{FALSE}, values of \code{table} once
  matched are excluded from the search for subsequent matches.  This
  behaviour is equivalent to the \R algorithm for argument
  matching, except for the consideration of empty strings (which in
  argument matching are matched after exact and partial matching to any
  remaining arguments).

  \code{\link{charmatch}} is similar to \code{pmatch} with
  \code{duplicates.ok} true, the differences being that it
  differentiates between no match and an ambiguous partial match,  it
  does match empty strings, and it does not allow multiple exact matches.
}
\value{
  A numeric vector of integers (including \code{NA} if \code{nomatch =
    NA}) of the same length as \code{x}, giving the indices of the
  elements in \code{table} which matched, or \code{nomatch}.
}
\note{
  Versions of \R prior to 1.0.0 had a different behaviour that was
  seriously incompatible with S (and the current version) when
  \code{duplicates.ok = TRUE}.
}
\author{
  Of this version, B. D. Ripley.
}
\seealso{
  \code{\link{match}}, \code{\link{charmatch}} and
  \code{\link{match.arg}}, \code{\link{match.fun}},
  \code{\link{match.call}}, for function argument matching etc.
}
\examples{
pmatch("", "")                             # returns NA
pmatch("m",   c("mean", "median", "mode")) # returns NA
pmatch("med", c("mean", "median", "mode")) # returns 2

pmatch(c("", "ab", "ab"), c("abc", "ab"), dup=F)
pmatch(c("", "ab", "ab"), c("abc", "ab"), dup=T)
## compare
charmatch(c("", "ab", "ab"), c("abc", "ab"))
}
\keyword{character}
