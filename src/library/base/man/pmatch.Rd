\name{pmatch}
\title{Partial String Matching}
\usage{
pmatch(x, table, nomatch = NA, duplicates.ok = FALSE)
}
\alias{pmatch}
\arguments{
  \item{x}{the values to be matched.}
  \item{table}{the values to be matched against.}
  \item{nomatch}{the value returned at non-matching positions.}
  \item{duplicates.ok}{should duplicate matches be allowed?}
}
\description{
  \code{pmatch} seeks matches for the elements of its first argument
  among those of its second.
}
\details{
  If there is a no match, then the value of \code{nomatch}
  is returned and if there is a unique match then the index
  of the matching value is returned.

  If there are multiple matches the result depends
  on the value of \code{duplicates.ok}.
  If this is false multiple matches will result
  in the value of \code{nomatch} being returned, and if it
  is true, the index of the first matching value will be returned.
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
}
\keyword{character}
