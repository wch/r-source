% File src/library/base/man/debug.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{debug}
\title{Debug a Function}
\alias{debug}
\alias{debugonce}
\alias{undebug}
\alias{isdebugged}
\alias{debuggingState}
\usage{
debug(fun, text = "", condition = NULL, signature = NULL)
debugonce(fun, text = "", condition = NULL, signature = NULL)
undebug(fun, signature = NULL)
isdebugged(fun, signature = NULL)
debuggingState(on = NULL)
}
\arguments{
  \item{fun}{any interpreted \R function.}
  \item{text}{a text string that can be retrieved when the browser is entered.}
  \item{condition}{a condition that can be retrieved when the browser is
    entered.}
  \item{signature}{an optional method signature. If specified, the
    method is debugged, rather than its generic.}
  \item{on}{logical; a call to the support function
    \code{debuggingState} returns \code{TRUE} if debugging is globally
    turned on, \code{FALSE} otherwise.  An argument of one or the other
    of those values sets the state.  If the debugging state is
    \code{FALSE}, none of the debugging actions will occur (but explicit
    \code{\link{browser}} calls in functions will continue to work).}
  %% can we describe what happens when it is turned off *inside* debugging ??
}
\description{
  Set, unset or query the debugging flag on a function.
  The \code{text} and \code{condition} arguments are the same as those
  that can be supplied via a call to \code{browser}. They can be retrieved
  by the user once the browser has been entered, and provide a mechanism to
  allow users to identify which breakpoint has been activated.
}
\details{
  When a function flagged for debugging is entered, normal execution
  is suspended and the body of function is executed one statement at a
  time.  A new browser context is initiated for each step (and the
  previous one destroyed).

  At the debug prompt the user can enter commands or \R expressions,
  followed by a newline.  The commands are described in the
  \code{\link{browser}} help topic.

  To debug a function which is defined inside another function,
  single-step though to the end of its definition, and then call
  \code{debug} on its name.

  If you want to debug a function not starting at the very beginning,
  use \code{\link{trace}(..., at = *)} or \code{\link{setBreakpoint}}.

  Using \code{debug} is persistent, and unless debugging is turned off
  the debugger will be entered on every invocation (note that if the
  function is removed and replaced the debug state is not preserved).
  Use \code{debugonce} to enter the debugger only the next time the
  function is invoked.
  
  To debug an S4 method by explicit signature, use
  \code{signature}. When specified, signature indicates the method of
  \code{fun} to be debugged. Note that debugging is implemented slightly
  differently for this case, as it uses the trace machinery, rather than
  the debugging bit. As such, \code{text} and \code{condition} cannot be
  specified in combination with a non-null \code{signature}. For methods
  which implement the \code{.local} rematching mechanism, the
  \code{.local} closure itself is the one that will be ultimately
  debugged (see \code{\link[methods]{isRematched}}).
  
  \code{isdebugged} returns \code{TRUE} if a) \code{signature} is \code{NULL}
  and the closure \code{fun} has been debugged, or b) \code{signature} is not
  \code{NULL}, \code{fun} is an S4 generic, and the method of \code{fun}
  for that signature has been debugged. In all other cases, it returns
  \code{FALSE}.
  
  The number of lines printed for the deparsed call when a function is
  entered for debugging can be limited by setting
  \code{\link{options}(deparse.max.lines)}.

  When debugging is enabled on a byte compiled function then the
  interpreted version of the function will be used until debugging is
  disabled.
}
\value{
  \code{debug} and \code{undebug} invisibly return \code{NULL}.

  \code{isdebugged} returns \code{TRUE} if the function or method is
  marked for debugging, and \code{FALSE} otherwise.
}
\seealso{
  \code{\link{debugcall}} for conveniently debugging methods,
  \code{\link{browser}}, \code{\link{trace}};
  \code{\link{traceback}} to see the stack after an \code{Error: \dots}
  message; \code{\link{recover}} for another debugging approach.
}

\examples{
\dontrun{
debug(library)
library(methods)
}
\dontrun{
debugonce(sample)
## only the first call will be debugged
sampe(10, 1)
sample(10, 1)
}
}
\keyword{programming}
\keyword{environment}
