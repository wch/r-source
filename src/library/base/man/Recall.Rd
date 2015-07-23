% File src/library/base/man/Recall.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

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
\note{
  \code{Recall} will not work correctly when passed as a function
  argument, e.g.\sspace{}to the \code{apply} family of functions.
}
\seealso{
  \code{\link{do.call}} and \code{\link{call}}.

  \code{\link{local}} for another way to write anonymous recursive functions.
}
\examples{
## A trivial (but inefficient!) example:
fib <- function(n)
   if(n<=2) { if(n>=0) 1 else 0 } else Recall(n-1) + Recall(n-2)
fibonacci <- fib; rm(fib)
## renaming wouldn't work without Recall
fibonacci(10) # 55
}
\keyword{programming}

