\name{pmatch}
\title{Partial String Matching}
\usage{
   pmatch(x, table, nomatch = NA)
charmatch(x, table, nomatch = NA)
}
\alias{pmatch}
\alias{charmatch}
\arguments{
\item{x}{the values to be matched.}
\item{table}{the values to be matched against.}
\item{nomatch}{the value returned at non-matching positions.}
}
\description{
\code{pmatch} seeks matches for the elements of its first argument
among those of its second.  If there is a unique match then the index
of the matching value is returned; if multiple matches are found then
\code{0} is returned and if no match is found then \code{NA} is
returned.

This function is based on Terry Therneau's \code{charmatch} S function.
In \R, \code{charmatch} is synonym to \code{pmatch}.
}
\seealso{
\code{\link{match}}.
}
\examples{
pmatch("m",   c("mean", "median", "mode")) # returns 0
pmatch("med", c("mean", "median", "mode")) # returns 2
}
\keyword{character}
