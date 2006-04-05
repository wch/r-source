\name{Recall}
\alias{Recall}
\title{Recursive Calling}
\usage{
Recall(\dots)
}
\arguments{
  \item{\dots}{all the arguments to be passed.}
}
\description{
  \code{Recall} is used as a placeholder for the name of the function
  in which it is called.  It allows the definition of recursive
  functions which still work after being renamed, see example below.
}
\details{
  Because of the way contexts in \R are implemented, this looks up the
  function from the name in the environment in which the function was
  originally called, and calls the function of that name which is found
  by the usual lookup rules.

  Beware that if \code{Recall} is called from within an S3 method, it
  will call the method and not the generic, and the method may not be
  visible in the environment from which the generic was called.
}
\note{
  \code{Recall} will not work correctly when passed as a function
  argument, eg to the \code{apply} family of functions.
}
\seealso{
  \code{\link{do.call}} and \code{\link{call}}.
    
  \code{\link{local}} for another way to write anonymous
  recursive functions
}
\examples{
## A trivial (but inefficient!) example:
fib <- function(n) if(n<=2) {if(n>=0) 1 else 0} else Recall(n-1) + Recall(n-2)
fibonacci <- fib; rm(fib)
## renaming wouldn't work without Recall
fibonacci(10) # 55
}
\keyword{programming}

