\name{force}
\alias{force}
\title{Force evaluation of an Argument}
\description{
  Forces the evaluation of a function argument.
}
\usage{
force(x)
}
\arguments{
  \item{x}{a formal argument.}
}
\details{
  \code{force} forces the evaluation of a formal argument.  This can
  be useful if the argument will be captured in a closure by the lexical
  scoping rules and will later be altered by an explicit assignment or
  an implicit assignment in a loop or an apply function.
}
\note{
  \code{force} does not force \link[delay]{promises}.
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
}
\keyword{data}
\keyword{programming}
