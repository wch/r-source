\name{names}
\title{The Names Attribute of an Object}
\usage{
names(x)
names(x) <- value
}
\alias{names}
\description{
\code{names} provides access to the names attribute of a vector.
The first form prints the names of the observations
and the second sets the names.
In this case, \code{value} must be a vector of character strings of the
same length as \code{x}.
}
\examples{
# print the names attribute of the islands data set
names(islands)

# remove the names attribute
names(islands) <- NULL
}
\keyword{attribute}
