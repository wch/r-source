\name{attributes}
\title{Object Attribute Lists}
\usage{
attributes(obj)
attributes(obj) <- list
}
\alias{attributes}
\alias{attributes<-}
\description{
This function provides access to an object's attribute list.
The simple form above returns the an object's attribute list.
The assignment form makes the list on the right-hand
side of the assignment, the object's attribute list.
}
\seealso{
\code{\link{attr}}.
}
\examples{
x <- cbind(a=1:3, pi=pi) # simple matrix w/ dimnames
attributes(x)

# strip an objects attributes:
attributes(x) <- NULL
x # now just a vector of length 6
}
\keyword{attribute}
