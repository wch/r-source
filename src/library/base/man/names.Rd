\name{names}
\title{The Names Attribute of an Object}
\usage{
names(x, \dots)
names.default(x)
names(x) <- value
}
\alias{names}
\alias{names.default}
\alias{names<-}
\alias{names<-.default}
\description{
\code{names} is a generic accessor function to the \code{names}
attribute of an \R object, typically a \code{\link{vector}}.
The first form prints the names of the observations
and the second sets the names.
In this case, \code{value} must be a vector of character strings of the
same length as \code{x}.
}
\examples{
\testonly{data(islands)}
# print the names attribute of the islands data set
names(islands)

# remove the names attribute
names(islands) <- NULL
}
\keyword{attribute}
