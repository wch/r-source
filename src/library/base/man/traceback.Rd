\name{traceback}
\alias{traceback}
\alias{.Traceback}
\title{Print Call Stacks}
\description{
  By default, \code{traceback()} prints the call stack of the last
  uncaught error, i.e., the sequence of calls that lead to the error.
  This is useful when an error occurs with an unidentifiable error
  message.  It can also be used to print arbitrary lists of deparsed
  calls.
}
\usage{
traceback(x = NULL)
}
\arguments{
  \item{x}{\code{NULL} (default), or a list or pairlist of deparsed
    calls.}
}
\value{
  \code{traceback()} returns nothing, but prints the deparsed call stack
  deepest call first.  The calls may print on more that one line, and
  the first line is labelled by the frame number.
}
\details{
  The stack of the last uncaught error is stored as a list of deparsed
  calls in \code{.Traceback}, which \code{traceback} prints in a
  user-friendly format.

  Errors which are caught \emph{via} \code{\link{try}} or
  \code{\link{tryCatch}} do not generate a traceback, so what is printed
  is the call sequence for the last uncaught error, and not necessarily
  the last error.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
