% File src/library/base/man/browser.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{browser}
\alias{browser}
\title{Environment Browser}
\description{
  Interrupt the execution of an expression and allow the inspection of
  the environment where \code{browser} was called from.
}
\usage{
browser(text = "", condition = NULL, expr = TRUE, skipCalls = 0L)
}
\arguments{
  \item{text}{a text string that can be retrieved once the browser is invoked.}
  \item{condition}{a condition that can be retrieved once the browser is
    invoked.}
  \item{expr}{An expression, which if it evaluates to \code{TRUE} the
    debugger will invoked, otherwise control is returned directly.}
  \item{skipCalls}{how many previous calls to skip when reporting the
    calling context.}
}
\details{
  A call to \code{browser} can be included in the body of a function.
  When reached, this causes a pause in the execution of the
  current expression and allows access to the \R interpreter.

  The purpose of the \code{text} and \code{condition} arguments are to
  allow helper programs (e.g., external debuggers) to insert specific
  values here, so that the specific call to browser (perhaps its location
  in a source file) can be identified and special processing can be
  achieved.  The values can be retrieved by calling \code{\link{browserText}}
  and \code{\link{browserCondition}}.

  The purpose of the \code{expr} argument is to allow for the illusion
  of conditional debugging. It is an illusion, because execution is
  always paused at the call to browser, but control is only passed
  to the evaluator described below if \code{expr} evaluates to \code{TRUE}.
  In most cases it is going to be more efficient to use an \code{if}
  statement in the calling program, but in some cases using this argument
  will be simpler.

  The \code{skipCalls} argument should be used when the \code{browser()}
  call is nested within another debugging function:  it will look further
  up the call stack to report its location.

  At the browser prompt the user can enter commands or \R expressions,
  followed by a newline.  The commands are

  \describe{
    \item{\code{c}}{exit the browser
    and continue execution at the next statement.}
    \item{\code{cont}}{synonym for \code{c}.}
    \item{\code{f}}{finish execution of the current loop or function} 
    \item{\code{help}}{print this list of commands}
    \item{\code{n}}{evaluate the next statement, stepping over 
      function calls.  For byte compiled functions interrupted by
      \code{browser} calls, \code{n} is equivalent to \code{c}.}
    \item{\code{s}}{evaluate the next statement, stepping into
      function calls.  Again, byte compiled functions make
      \code{s} equivalent to \code{c}.}
    \item{\code{where}}{print a stack trace of all active function calls.}
    \item{\code{Q}}{exit the browser and the current evaluation and
      return to the top-level prompt.}
  }
  Leading and trailing whitespace is ignored, except for an empty line.
  Handling of empty lines depends on the \code{"browserNLdisabled"} 
  \link[=options]{option}; if it is \code{TRUE}, empty lines are ignored.  
  If not, an empty line is the same as \code{n} (or \code{s}, if it was used 
  most recently).

  Anything else entered at the browser prompt is interpreted as an
  \R expression to be evaluated in the calling environment: in
  particular typing an object name will cause the object to be printed,
  and \code{ls()} lists the objects in the calling frame.  (If you want
  to look at an object with a name such as \code{n}, print it
  explicitly.)

  The number of lines printed for the deparsed call can be limited by
  setting \code{\link{options}(deparse.max.lines)}.
  
  The browser prompt is of the form \code{Browse[\var{n}]>}: here
  \code{var{n}} indicates the \sQuote{browser level}.  The browser can
  be called when browsing (and often is when \code{\link{debug}} is in
  use), and each recursive call increases the number.  (The actual
  number is the number of \sQuote{contexts} on the context stack: this
  is usually \code{2} for the outer level of browsing and \code{1} when
  examining dumps in \code{\link{debugger}}.)
  
  This is a primitive function but does argument matching in the
  standard way.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{debug}}, and
  \code{\link{traceback}} for the stack on error.
  \code{\link{browserText}} for how to retrieve the text and condition.
}
\keyword{programming}
\keyword{environment}
