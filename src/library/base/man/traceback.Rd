\name{traceback}
\title{Print Call Stack of Last Error}
\usage{
traceback()
}
\alias{traceback}
\alias{.Traceback}
\description{
  \code{traceback()} prints the call stack of the last error, i.e., the
  sequence of calls that lead to the error.  This is useful when an
  error occurs with an unidentifiable error message.
  Currently, this stack is given as a list in \code{.Traceback}.
}
\examples{
foo <- function(x) { print(1); bar(2) }
bar <- function(x) { x + a.variable.which.does.not.exist }
foo(2) # gives a strange error
traceback()
## [1] "bar(2)" "foo(2)"
bar
## Ah, this is the culprit ...
}
\keyword{programming}
