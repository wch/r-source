\name{charmatch}
\title{Partial String Matching}
\usage{
charmatch(x, table, nomatch = NA)
}
\alias{charmatch}
\arguments{
\item{x}{the values to be matched.}
\item{table}{the values to be matched against.}
\item{nomatch}{the value returned at non-matching positions.}
}
\description{
\code{charmatch} seeks matches for the elements of its first argument
among those of its second.  If there is a unique match then the index
of the matching value is returned; if multiple matches are found then
\code{0} is returned and if no match is found then \code{NA} is
returned.

This function is based on a C function written by Terry Therneau.
}
\seealso{
\code{\link{match}} \code{\link{pmatch}}.
}
\examples{
charmatch("", "")                             # returns 1
charmatch("m",   c("mean", "median", "mode")) # returns 0
charmatch("med", c("mean", "median", "mode")) # returns 2
}
\keyword{character}

