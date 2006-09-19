\name{debug}
\title{Debug a Function}
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
  is suspended and the body of function is executed one statement at a
  time.  A new browser context is initiated for each step (and the
  previous one destroyed).

  At the debug prompt the user can enter commands or \R expressions.
  The commands are

  \describe{
    \item{\code{n}}{(or just return).  Advance to the next step.}
    \item{\code{c}}{continue to the end of the current context: e.g. to the
      end of the loop if within a loop or to the end of the function.}
    \item{\code{cont}}{synonym for \code{c}.}
    \item{\code{where}}{print a stack trace of all active function calls.}
    \item{\code{Q}}{exit the browser and the current evaluation and
      return to the top-level prompt.}
  }
  (Leading and trailing whitespace is ignored, except for return).

  Anything else entered at the debug prompt is interpreted as an
  \R expression to be evaluated in the calling environment: in
  particular typing an object name will cause the object to be printed,
  and \code{ls()} lists the objects in the calling frame.  (If you want
  to look at an object with a name such as \code{n}, print it explicitly.)

  If a function is defined inside a function, single-step though to the
  end of its definition, and then call \code{debug} on its name.
  
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
