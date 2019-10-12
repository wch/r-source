% File src/library/base/man/traceback.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{traceback}
\title{Get and Print Call Stacks}
\alias{traceback}
\alias{.traceback}
\alias{.Traceback}
\description{
  By default \code{traceback()} prints the call stack of the last
  uncaught error, i.e., the sequence of calls that lead to the error.
  This is useful when an error occurs with an unidentifiable error
  message.  It can also be used to print the current stack or
  arbitrary lists of calls.

  \code{.traceback()} now \emph{returns} the above call stack (and
  \code{traceback(x, *)} can be regarded as convenience function for
  printing the result of \code{.traceback(x)}).
}
\usage{
 traceback(x = NULL, max.lines = getOption("traceback.max.lines",
                                           getOption("deparse.max.lines", -1L)))
.traceback(x = NULL, max.lines = getOption("traceback.max.lines",
                                           getOption("deparse.max.lines", -1L)))
}
\arguments{
  \item{x}{\code{NULL} (default, meaning \code{.Traceback}), or an
    integer count of calls to skip in the current stack, or a list or
    pairlist of calls.  See the details.}
  \item{max.lines}{a number, the maximum number of lines to be printed
    \emph{per call}.  The default is unlimited.  Applies only when \code{x}
    is \code{NULL}, a \code{\link{list}} or a \code{\link{pairlist}} of
    calls, see the details.}
}
\details{
  The default display is of the stack of the last uncaught error as
  stored as a list of \code{\link{call}}s in \code{.Traceback}, which
  \code{traceback} prints in a user-friendly format.  The stack of
  calls always contains all function calls and all foreign
  function calls (such as \code{\link{.Call}}): if profiling is in
  progress it will include calls to some primitive functions.  (Calls
  to builtins are included, but not to specials.)

  Errors which are caught \emph{via} \code{\link{try}} or
  \code{\link{tryCatch}} do not generate a traceback, so what is printed
  is the call sequence for the last uncaught error, and not necessarily
  for the last error.

  If \code{x} is numeric, then the current stack is printed, skipping
  \code{x} entries at the top of the stack.  For example,
  \code{options(error = function() traceback(3))} will print the stack
  at the time of the error, skipping the call to \code{traceback()} and
  \code{.traceback()}
  and the error function that called it.

  Otherwise, \code{x} is assumed to be a list or pairlist of calls or
  deparsed calls and will be displayed in the same way.

  \code{.traceback()} and by extension \code{traceback()} may trigger
  deparsing of \code{\link{call}}s.  This is an expensive operation
  for large calls so it may be advisable to set \code{max.lines}
  to a reasonable value when such calls are on the call stack.
}
\value{
  \code{.traceback()} returns the deparsed call stack deepest call
  first as a list or pairlist.  The number of lines deparsed from
  the call can be limited via \code{max.lines}.  Calls for which
  \code{max.lines} results in truncated output will gain a
  \code{\dQuote{truncated}} attribute.

  \code{traceback()} formats, prints, and returns the call stack
  produced by \code{.traceback()} invisibly.
}
\section{Warning}{
  It is undocumented where \code{.Traceback} is stored nor that it is
  visible, and this is subject to change.  As of \R 4.0.0
  \code{.Traceback} contains the \code{\link{call}}s as language
  objects, whereas previously these calls were deparsed.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
foo <- function(x) { print(1); bar(2) }
bar <- function(x) { x + a.variable.which.does.not.exist }
\dontrun{
foo(2) # gives a strange error
traceback()}%dont
## 2: bar(2)
## 1: foo(2)
bar
## Ah, this is the culprit ...

## This will print the stack trace at the time of the error.
options(error = function() traceback(3))
}
\keyword{programming}
