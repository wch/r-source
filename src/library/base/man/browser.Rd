\name{browser}
\alias{browser}
\title{Environment Browser}
\description{
  Interrupt the execution of an expression and allow the inspection of
  the environment where \code{browser} was called from.
}
\usage{
browser()
}
\details{
  A call to \code{browser} causes a pause in the execution of the
  current expression and runs a copy of the \R interpreter which has
  access to variables local to the environment where the call took
  place.

  Local variables can be listed with \code{ls}, and manipulated with \R
  expressions typed to this sub-interpreter.  The sub-interpreter can be
  exited by typing \code{c}.  Execution then resumes at the statement
  following the call to \code{browser}.

  Typing \code{n} causes the step-through-debugger, to start and it is
  possible to step through the remainder of the function one line at a
  time.  In this mode \code{c} will continue to the end of the current
  context (to the next loop iteration if within a loop).

  Typing \code{Q} quits the current execution and returns you to the
  top-level prompt.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{debug}}, and
  \code{\link{traceback}} for the stack on error.
}
\keyword{programming}
\keyword{environment}
