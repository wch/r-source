\name{get}
\title{Return a Variable's Value}
\usage{
get(x, pos=-1, envir=NULL, mode="any", inherits=TRUE)
}
\alias{get}
\arguments{
\item{x}{a variable name (given as a quoted string).}
\item{envir}{the environent to be used.}
\item{mode}{the type of object sought.}
\item{inherits}{should the enclosing frames of
the environment be inspected?}
}
\value{
This function searches the specified environment for a
bound variable whose name is given by the string \code{x}.
If the variable's value is not of the correct
\code{mode}, it is ignored.

If \code{inherits} is \code{FALSE}, only the first frame
of the specified environment is inspected.
If \code{inherits} is \code{TRUE}, the search is continued
up through the parent frames until a bound value of the right
mode is found.

Using a \code{NULL} environment is equivalent to using
the current environment.
}
\seealso{
\code{\link{exists}}.
}
\examples{
get("\%o\%")
}
\keyword{data}
