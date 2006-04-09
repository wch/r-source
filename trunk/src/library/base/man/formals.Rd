\name{formals}
\alias{formals}
\alias{formals<-}
\title{Access to and Manipulation of the Formal Arguments}
\description{
  Get or set the formal arguments of a function.
}
\usage{
formals(fun = sys.function(sys.parent()))
formals(fun, envir = parent.frame()) <- value
}
\arguments{
  \item{fun}{a function object, or see Details.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{a list of \R expressions.}
}
\value{
  \code{formals} returns the formal argument list of the function
  specified.

  The assignment form sets the formals of a function to the list on the
  right hand side.
}
\details{
  For the first form, \code{fun} can be a character string
  naming the function to be manipulated, which is searched for from the
  parent environment.
  If it is not specified, the function calling \code{formals} is used.
}
\seealso{
  \code{\link{args}} for a \dQuote{human-readable} version,
  \code{\link{alist}},
  \code{\link{body}},
  \code{\link{function}}.
}
\examples{
length(formals(lm))      # the number of formal arguments
names(formals(boxplot))  # formal arguments names

f <- function(x)a+b
formals(f) <- alist(a=,b=3) # function(a,b=3)a+b
f(2) # result = 5
}
\keyword{programming}
