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
\examples{
# These functions both return their argument
# The value returned by the second does not print
f1 <- function(x) x
f2 <- function(x) invisible(x)
}
\keyword{programming}
