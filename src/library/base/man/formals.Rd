\name{formals}
\alias{formals}
\alias{formals<-}
\title{Access to and Manipulation of the Formal Arguments}
\description{
  Get or set the formal arguments of a function.
}
\usage{
formals(fun = sys.function(sys.parent()))
formals(fun) <- list
}
\arguments{
  \item{fun}{a function object or a character string naming the function
    to be manipulated.
    If not specified, the function calling \code{body} is used.}
  \item{list}{a list of \R expressions.}
}
\value{
  \code{formals} returns the formal argument list of the function
  specified.

  The assignment form sets the formals of a function to the list on the
  right hand side.
}
\seealso{
  \code{\link{args}} for a ``human-readable'' version,
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
