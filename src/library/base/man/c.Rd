\name{c}
\title{Combine Values into a Vector or List}
\usage{
c(\dots, recursive=FALSE)
}
\alias{c}
\description{
This is a generic function which combines its arguments.

The default method combines its arguments to form a vector.
All arguments are coerced to a common type which is the type
of the returned value.
If \code{recursive=TRUE}, the function recursively
descends through lists combining all their elements into
a vector.
}
\seealso{
\code{\link{unlist}}.
}
\examples{
c(1,7:9)
c(1:5, 10.5, "next")

c(list(A=c(B=1)),recursive=T)

c(options(), recursive=T)
c(list(A=c(B=1,C=2),B=c(E=7)),recursive=T)
}
\keyword{manip}
