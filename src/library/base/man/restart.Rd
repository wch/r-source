\name{restart-deprecated}
\alias{restart-deprecated}
\title{Restart an Expression}
\usage{
restart(on = TRUE)
}
\arguments{
  \item{on}{if true a jump point is set; if false the jump point is removed.}
}
\description{
  \code{restart} performs a type of non-local return.
}
\details{
  When \code{restart} is called with \code{on = TRUE} the evaluator
  marks that function as a return point.  Any errors or signals
  (such as control-C on Unix) cause control to return to the start of
  the function containing the call to \code{restart}. The most recently
  established function is always entered first.
}
\note{
  The direct use of \code{restart} is likely to result in an infinite loop.
  Use \code{\link{try}} unless you are \bold{sure} you know what
  you are doing.

  In fact, as \code{restart} is deprecated, don't use it  even if you
  \bold{are} sure you know what you are doing.
}
\seealso{
  \code{\link{try}} for a safer interface.
  
  \code{\link{options}} for setting error handlers and suppressing the
  printing of error messages.
}
\keyword{programming}
