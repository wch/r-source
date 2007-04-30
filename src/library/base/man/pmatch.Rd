\name{pmatch}
\title{Partial String Matching}
\usage{
pmatch(x, table, nomatch = NA, duplicates.ok = FALSE)
}
\alias{pmatch}
\arguments{
  \item{x}{the values to be matched: converted to a character vector by
    \code{\link{as.character}}.}
  \item{table}{the values to be matched against: converted to a character
    vector.}
  \item{nomatch}{the (integer) value to be returned at non-matching or
    multiply partially matching positions.}
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
  partial matches are considered, and if found recorded. (A partial
  match occurs if the whole of the element of \code{x} matches the
  beginning of the element of \code{table}.)  Finally,
  all remaining elements of \code{x} are regarded as unmatched.
  In addition, an empty string can match nothing, not even an exact
  match to an empty string.  This is the appropriate behaviour for
  partial matching of character indices, for example.

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

  \code{NA} values are treated as if they were the string constant \code{"NA"}.
}
\value{
  An integer vector (possibly including \code{NA} if \code{nomatch =
    NA}) of the same length as \code{x}, giving the indices of the
  elements in \code{table} which matched, or \code{nomatch}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{match}}, \code{\link{charmatch}} and
  \code{\link{match.arg}}, \code{\link{match.fun}},
  \code{\link{match.call}}, for function argument matching etc.,
  \code{\link{grep}} etc for more general (regexp) matching of strings.
}
\examples{
pmatch("", "")                             # returns NA
pmatch("m",   c("mean", "median", "mode")) # returns NA
pmatch("med", c("mean", "median", "mode")) # returns 2

pmatch(c("", "ab", "ab"), c("abc", "ab"), dup=FALSE)
pmatch(c("", "ab", "ab"), c("abc", "ab"), dup=TRUE)
## compare
charmatch(c("", "ab", "ab"), c("abc", "ab"))
}
\keyword{character}
