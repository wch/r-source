\name{formals}
\title{Access to and Manipulation of the Formal Arguments}
\usage{
formals(fun=sys.function(sys.parent()))
formals(obj) <- list
}
\alias{formals}
\alias{formals<-}
\value{
If \code{fun} is a function then the formal argument list to that
function is returned.
If \code{fun} is a character string then the function with that name
is found and used.
If \code{fun} is not specified then the function calling \code{formals}
is used.

The assignment form sets the formals of a function to the list on the
right hand side.
}
\seealso{
\code{\link{args}} for a ``human--readable'' version,
\code{\link{alist}}, \code{\link{body}}, \code{\link{function}}.
}
\examples{
length(formals(lm))      # the number of formal arguments
names(formals(boxplot))  # formal arguments names

f <- function(x)a+b
formals(f) <- alist(a=,b=3) # function(a,b=3)a+b
f(2) # result = 5
}
\keyword{programming}
