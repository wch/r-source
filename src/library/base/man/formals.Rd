\name{formals}
\title{Access to the Formal Arguments}
\usage{
formals(fun=sys.function(sys.parent()))
}
\alias{formals}
\value{
If \code{fun} is a function then the formal argument list to that
function is returned.
If \code{fun} is a character string then the function with that name
is found and used.
If \code{fun} is not specified then the function calling \code{formals}
is used.

It is currently not possible to set the formals of a function.
}
\seealso{
\code{\link{args}}
}
\examples{
length(formals(lm))      # the number of formal arguments
names(formals(boxplot))  # formal arguments names
}
\keyword{programming}
