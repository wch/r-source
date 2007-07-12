\name{charmatch}
\title{Partial String Matching}
\usage{
charmatch(x, table, nomatch = NA_integer_)
}
\alias{charmatch}
\arguments{
  \item{x}{the values to be matched: converted to a character vector by
    \code{\link{as.character}}.}
  \item{table}{the values to be matched against: converted to a character
    vector.}
  \item{nomatch}{the (integer) value to be returned at non-matching
    positions.}
}
\description{
  \code{charmatch} seeks matches for the elements of its first argument
  among those of its second.
}
\details{
  Exact matches are preferred to partial matches (those where the value
  to be matched has an exact match to the initial part of the target,
  but the target is longer).

  If there is a single exact match or no exact match and a unique
  partial match then the index of the matching value is returned; if
  multiple exact or multiple partial matches are found then \code{0} is
  returned and if no match is found then \code{nomatch} is returned.

  \code{NA} values are treated as the string constant \code{"NA"}.
}
\value{
  An integer vector of the same length as \code{x}, giving the
  indices of the elements in \code{table} which matched, or \code{nomatch}.
}
\author{
  This function is based on a C function written by Terry Therneau.
}
\seealso{
  \code{\link{pmatch}}, \code{\link{match}}.

  \code{\link{grep}} or \code{\link{regexpr}} for more general (regexp)
  matching of strings.
}
\examples{
charmatch("", "")                             # returns 1
charmatch("m",   c("mean", "median", "mode")) # returns 0
charmatch("med", c("mean", "median", "mode")) # returns 2
}
\keyword{character}

