\name{attr}
\title{Object Attributes}
\usage{
attr(x, which)
attr(x, which) <- value
}
\alias{attr}
\alias{attr<-}
\arguments{
\item{x}{an object whose attributes are to be accessed.}
\item{which}{a character string specifying which attribute is to be accessed.}
}
\value{
This function provides access to a single object attribute.  The simple
form above returns the value of the named attribute.  The assignment
form causes the named attribute to take the value on the right of
the assignment symbol.
}
\seealso{
\code{\link{attributes}}
}
\examples{
# create a 2 by 5 matrix
x <- 1:10
attr(x,"dim") <- c(2,5)
}
\keyword{attribute}
