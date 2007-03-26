\name{force}
\alias{force}
\title{Force Evaluation of an Argument}
\description{
  Forces the evaluation of a function argument.
}
\usage{
force(x)
}
\arguments{
  \item{x}{a formal argument of the enclosing function.}
}
\details{
  \code{force} forces the evaluation of a formal argument.  This can
  be useful if the argument will be captured in a closure by the lexical
  scoping rules and will later be altered by an explicit assignment or
  an implicit assignment in a loop or an apply function.
}
\note{
  This is semantic sugar: just evaluating the symbol will do the
  same thing (see the examples).
  
  \code{force} does not force the evaluation of other
  \link[=delayedAssign]{promises}.  (It works by forcing the promise that
  is created when the actual arguments of a call are matched to the
  formal arguments of a closure, the mechanism which implements
  \emph{lazy evaluation}.)
}
\examples{
f <- function(y) function() y
lf <- vector("list", 5)
for (i in seq(along = lf)) lf[[i]] <- f(i)
lf[[1]]()  # returns 5

g <- function(y) { force(y); function() y }
lg <- vector("list", 5)
for (i in seq(along = lg)) lg[[i]] <- g(i)
lg[[1]]()  # returns 1

## This is identical to
g <- function(y) { y; function() y }
}
\keyword{data}
\keyword{programming}
