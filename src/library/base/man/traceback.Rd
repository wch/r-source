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
  This stack is stored as a list in \code{.Traceback},
  which \code{traceback} prints in a user-friendly format.
}
\value{
  \code{traceback()} returns nothing, but prints the deparsed call stack
  deepest call first.  The calls may print on more that one line, and
  the first line is labelled by the frame number.
}
\examples{
foo <- function(x) { print(1); bar(2) }
bar <- function(x) { x + a.variable.which.does.not.exist }
\dontrun{
foo(2) # gives a strange error
traceback()}
## 2: bar(2)
## 1: foo(2)
bar
## Ah, this is the culprit ...
}
\keyword{programming}
