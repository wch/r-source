\name{invisible}
\title{Change the Print Mode to Invisible}
\usage{invisible(expr)}
\alias{invisible}
\value{
This function returns a (temporarily) invisible copy of its argument.
This can be useful when it is desired to have functions
return values which can be assigned, but which do not
print when they are not assigned.
}
\seealso{\code{\link{return}} and \code{\link{function}}.}
\examples{
# These functions both return their argument
f1 <- function(x) x
f2 <- function(x) invisible(x)
f1(1)# prints
f2(1)# does not
}
\keyword{programming}
