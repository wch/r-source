\name{exists}
\title{Is a Variable Defined?}
\usage{
exists(x, where =NULL, envir=sys.frame(sys.parent()), frame =NULL,
       mode = "any", inherits = TRUE)
}
\alias{exists}
\arguments{
\item{x}{a variable name (given as a quoted string).}
\item{where, envir, frame}{an environent to be searched. By default this is the
environment where the call to \code{envir} takes place.}
\item{mode}{the type of interest for the object.}
\item{inherits}{should the enclosing frames of the environment be inspected.}
}
\description{
This function looks to see if the name \code{x} has a value bound to it.
If \code{inherits} is \code{TRUE} and a value is not found for \code{x},
then the parent frames of \code{envir} are searched until the name
\code{x} is encountered.
If \code{mode} is specified then only objects of that mode are sought.
The function returns \code{TRUE} if the variable is encountered and
\code{FALSE} if not.
}
\seealso{
\code{\link{get}}.
}
\examples{
##  Define a substitute function if necessary:
if(!exists("some.fun", mode="function"))
 some.fun <- function(x) { cat("some.fun(x)\n"); x }
}
\keyword{data}
