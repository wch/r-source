\name{body}
\title{Access to and Manipulation of the Body of a Function}
\usage{
body(fun=sys.function(sys.parent()))
body(obj) <- list
}
\alias{body}
\alias{body<-}
\value{
If \code{fun} is a function then the body of that
function is returned.
If \code{fun} is a character string then the function with that name
is found and used.
If \code{fun} is not specified then the function calling \code{body}
is used.

The assignment form sets the body of a function \code{obj} to the
\code{list} on the right hand side.
}
\seealso{
\code{\link{alist}}, \code{\link{args}}, \code{\link{function}}.
}
\examples{
body(body)
f <- function(x) x^5
body(f) <- expression(5^x)
f(3) # = 125
str(body(f))
}
\keyword{programming}
