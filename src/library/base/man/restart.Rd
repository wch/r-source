\name{restart}
\alias{restart}
\alias{try}
\title{Restart an Expression}
\usage{
restart(on = TRUE)
try(expr, first = T)
}
\arguments{
  \item{on}{if true a jump point is set; if false the jump point is removed}
  \item{expr}{an \R expression to try}
  \item{first}{not for user use!}
}
\description{
  \code{restart} performs a type of non-local return.

  \code{try} is a user-friendly wrapper to run an expression that might fail.
}
\details{
  When \code{restart} is called with \code{on = TRUE} the evaluator marks that 
  function as a return point. Any errors or signals (such as control-C on
  Unix) cause control to return to the start of the function containing the
  call to \code{restart}. The most recently established function is always
  entered first.
}
\value{
  \code{try} returns the value of the expression if it succeeds, and an
  invisible object of class \code{"try_error"} containing the error
  message if it if fails. The normal error handling will print the same
  message unless \code{options("show.error.messages")} is false.
}
\note{
  The direct use of \code{restart} is likely to result in an infinite loop.
  Use \code{try} unless you are sure you know what you are doing.
}
\seealso{
  \code{\link{options}} for setting error handlers and suppressing the
  printing of error messages;
  \code{\link{geterrmessage}} for retrieving the last error message.
}
\examples{
## this example will not work correctly in example(try), but
##  it does work correctly if pasted in
options(show.error.messages = FALSE)
try(log("a"))
print(.Last.value)
options(show.error.messages = TRUE)
}
\keyword{programming}
