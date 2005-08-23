\name{debug}
\title{Debug a function}
\usage{
debug(fun)
undebug(fun)
}
\alias{debug}
\alias{undebug}
\arguments{
\item{fun}{any interpreted \R function.}
}
\description{
  Set or unset the debugging flag on a function.
}
\details{
  When a function flagged for debugging is entered, normal execution
  is suspended and the body of function is executed one statement at a time.
  A new browser context is initiated for each step (and the previous one
  destroyed).
  You take the next step by typing carriage return, \code{n} or \code{next}.
  You can see the values of variables by typing their names.
  Typing \code{c} or \code{cont} causes the debugger to continue to the
  end of the function (or loop if within a loop).
  You can \code{debug} new functions before you step in to them from inside
  the debugger.
  Typing \code{Q} quits the current execution and returns you to the
  top--level prompt.
  Typing \code{where} causes the debugger to print out the current
  stack trace (all functions that are active).
  If you have variables with names that are identical to the controls
  (eg. \code{c} or \code{n} ) then you need to use \code{print(c)} and
  \code{print(n)} to evaluate them.

  In order to debug S4 methods (see \code{\link[methods]{Methods}}), you
  need to use \code{\link{trace}}, typically calling \code{\link{browser}},
  e.g., as \cr
  \code{trace("plot", browser, exit=browser, signature = c("track", "missing"))}
}
\seealso{
  \code{\link{browser}}, \code{\link{trace}};
  \code{\link{traceback}} to see the stack after an \code{Error: \dots}
  message; \code{\link{recover}} for another debugging approach.
}
\keyword{programming}
\keyword{environment}
