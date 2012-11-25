
% File src/library/base/man/debug.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{debug}
\title{Debug a Function}
\usage{
debug(fun, text = "", condition = NULL)
debugonce(fun, text = "", condition = NULL)
undebug(fun)
isdebugged(fun)
}
\alias{debug}
\alias{debugonce}
\alias{undebug}
\alias{isdebugged}
\arguments{
\item{fun}{any interpreted \R function.}
\item{text}{a text string that can be retrieved when the browser is entered.}
\item{condition}{a condition that can be retrieved when the browser is entered.}
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
  followed by a newline.  The commands are

  \describe{
    \item{\code{n}}{(or just an empty line, by default).
      Advance to the next step.}
    \item{\code{c}}{continue to the end of the current context: e.g. to the
      end of the loop if within a loop or to the end of the function.}
    \item{\code{cont}}{synonym for \code{c}.}
    \item{\code{where}}{print a stack trace of all active function calls.}
    \item{\code{Q}}{exit the browser and the current evaluation and
      return to the top-level prompt.}
  }
  (Leading and trailing whitespace is ignored, except for an empty line).

  Anything else entered at the debug prompt is interpreted as an
  \R expression to be evaluated in the calling environment: in
  particular typing an object name will cause the object to be printed,
  and \code{ls()} lists the objects in the calling frame.  (If you want
  to look at an object with a name such as \code{n}, print it explicitly.)

  Setting \link[=options]{option} \code{"browserNLdisabled"} to \code{TRUE}
  disables the use of an empty line as a synonym for \code{n}.  If this
  is done, the user will be re-prompted for input until a valid command
  or an expression is entered.

  To debug a function is defined inside a function, single-step though
  to the end of its definition, and then call \code{debug} on its name.

  If you want to debug a function not starting at the very beginning,
  use \code{\link{trace}(..., at = *)} or \code{\link{setBreakpoint}}.

  Using \code{debug} is persistent, and unless debugging is turned off
  the debugger will be entered on every invocation (note that if the
  function is removed and replaced the debug state is not preserved).
  Use \code{debugonce} to enter the debugger only the next time the
  function is invoked.

  In order to debug S4 methods (see \code{\link{Methods}}), you
  need to use \code{\link{trace}}, typically calling \code{\link{browser}},
  e.g., as \cr
  \code{trace("plot", browser, exit = browser, signature = c("track", "missing"))}

  The number of lines printed for the deparsed call when a function is
  entered for debugging can be limited by setting
  \code{\link{options}(deparse.max.lines)}.

  When debugging is enabled on a byte compiled function then the
  interpreted version of the function will be used until debugging is
  disabled.
}
\seealso{
  \code{\link{browser}}, \code{\link{trace}};
  \code{\link{traceback}} to see the stack after an \code{Error: \dots}
  message; \code{\link{recover}} for another debugging approach.
}
\keyword{programming}
\keyword{environment}
