% File src/library/base/man/conditions.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{conditions}
\alias{conditions}
\alias{condition}
\alias{computeRestarts}
\alias{conditionCall}
\alias{conditionMessage}
\alias{findRestart}
\alias{invokeRestart}
\alias{invokeRestartInteractively}
\alias{isRestart}
\alias{restartDescription}
\alias{restartFormals}
\alias{signalCondition}
\alias{simpleCondition}
\alias{simpleError}
\alias{simpleWarning}
\alias{simpleMessage}
\alias{tryCatch}
\alias{withCallingHandlers}
\alias{withRestarts}

\alias{.signalSimpleWarning}
\alias{.handleSimpleError}
\alias{.tryResumeInterrupt}

\alias{as.character.condition}
\alias{as.character.error}
\alias{conditionCall.condition}
\alias{conditionMessage.condition}
\alias{print.condition}
\alias{print.restart}

\title{Condition Handling and Recovery}
\description{
  These functions provide a mechanism for handling unusual conditions,
  including errors and warnings.
}

\usage{
tryCatch(expr, \dots, finally)
withCallingHandlers(expr, \dots)

signalCondition(cond)

simpleCondition(message, call = NULL)
simpleError    (message, call = NULL)
simpleWarning  (message, call = NULL)
simpleMessage  (message, call = NULL)

\method{as.character}{condition}(x, \dots)
\method{as.character}{error}(x, \dots)
\method{print}{condition}(x, \dots)
\method{print}{restart}(x, \dots)

conditionCall(c)
\method{conditionCall}{condition}(c)
conditionMessage(c)
\method{conditionMessage}{condition}(c)

withRestarts(expr, \dots)

computeRestarts(cond = NULL)
findRestart(name, cond = NULL)
invokeRestart(r, \dots)
invokeRestartInteractively(r)

isRestart(x)
restartDescription(r)
restartFormals(r)

.signalSimpleWarning(msg, call)
.handleSimpleError(h, msg, call)
.tryResumeInterrupt()
}

\arguments{
  \item{c}{a condition object.}
  \item{call}{call expression.}
  \item{cond}{a condition object.}
  \item{expr}{expression to be evaluated.}
  \item{finally}{expression to be evaluated before returning or exiting.}
  \item{h}{function.}
  \item{message}{character string.}
  \item{msg}{character string.}
  \item{name}{character string naming a restart.}
  \item{r}{restart object.}
  \item{x}{object.}
  \item{\dots}{additional arguments; see details below.}
}

\details{

   The condition system provides a mechanism for signaling and
   handling unusual conditions, including errors and warnings.
   Conditions are represented as objects that contain information
   about the condition that occurred, such as a message and the call in
   which the condition occurred.  Currently conditions are S3-style
   objects, though this may eventually change.

   Conditions are objects inheriting from the abstract class
   \code{condition}.  Errors and warnings are objects inheriting
   from the abstract subclasses \code{error} and \code{warning}.
   The class \code{simpleError} is the class used by \code{stop}
   and all internal error signals.  Similarly, \code{simpleWarning}
   is used by \code{warning}, and \code{simpleMessage} is used by
   \code{message}.  The constructors by the same names take a string
   describing the condition as argument and an optional call.  The
   functions \code{conditionMessage} and \code{conditionCall} are
   generic functions that return the message and call of a condition.

   Conditions are signaled by \code{signalCondition}.  In addition,
   the \code{stop} and \code{warning} functions have been modified to
   also accept condition arguments.

   The function \code{tryCatch} evaluates its expression argument
   in a context where the handlers provided in the \code{\dots}
   argument are available.  The \code{finally} expression is then
   evaluated in the context in which \code{tryCatch} was called; that
   is, the handlers supplied to the current \code{tryCatch} call are
   not active when the \code{finally} expression is evaluated.

   Handlers provided in the \code{\dots} argument to \code{tryCatch}
   are established for the duration of the evaluation of \code{expr}.
   If no condition is signaled when evaluating \code{expr} then
   \code{tryCatch} returns the value of the expression.

   If a condition is signaled while evaluating \code{expr} then
   established handlers are checked, starting with the most recently
   established ones, for one matching the class of the condition.
   When several handlers are supplied in a single \code{tryCatch} then
   the first one is considered more recent than the second.  If a
   handler is found then control is transferred to the
   \code{tryCatch} call that established the handler, the handler
   found and all more recent handlers are disestablished, the handler
   is called with the condition as its argument, and the result
   returned by the handler is returned as the value of the
   \code{tryCatch} call.

   Calling handlers are established by \code{withCallingHandlers}.  If
   a condition is signaled and the applicable handler is a calling
   handler, then the handler is called by \code{signalCondition} in
   the context where the condition was signaled but with the available
   handlers restricted to those below the handler called in the
   handler stack.  If the handler returns, then the next handler is
   tried; once the last handler has been tried, \code{signalCondition}
   returns \code{NULL}.

   User interrupts signal a condition of class \code{interrupt} that
   inherits directly from class \code{condition} before executing the
   default interrupt action.

   Restarts are used for establishing recovery protocols.  They can be
   established using \code{withRestarts}.  One pre-established restart is
   an \code{abort} restart that represents a jump to top level.

   \code{findRestart} and \code{computeRestarts} find the available
   restarts.  \code{findRestart} returns the most recently established
   restart of the specified name.  \code{computeRestarts} returns a
   list of all restarts.  Both can be given a condition argument and
   will then ignore restarts that do not apply to the condition.

   \code{invokeRestart} transfers control to the point where the
   specified restart was established and calls the restart's handler with the
   arguments, if any, given as additional arguments to
   \code{invokeRestart}.  The restart argument to \code{invokeRestart}
   can be a character string, in which case \code{findRestart} is used
   to find the restart.

   New restarts for \code{withRestarts} can be specified in several ways.
   The simplest is in \code{name = function} form where the function is
   the handler to call when the restart is invoked.  Another simple
   variant is as \code{name = string} where the string is stored in the
   \code{description} field of the restart object returned by
   \code{findRestart}; in this case the handler ignores its arguments
   and returns \code{NULL}.  The most flexible form of a restart
   specification is as a list that can include several fields, including
   \code{handler}, \code{description}, and \code{test}.  The
   \code{test} field should contain a function of one argument, a
   condition, that returns \code{TRUE} if the restart applies to the
   condition and \code{FALSE} if it does not; the default function
   returns \code{TRUE} for all conditions.

   One additional field that can be specified for a restart is
   \code{interactive}.  This should be a function of no arguments that
   returns a list of arguments to pass to the restart handler.  The list
   could be obtained by interacting with the user if necessary.  The
   function \code{invokeRestartInteractively} calls this function to
   obtain the arguments to use when invoking the restart.  The default
   \code{interactive} method queries the user for values for the
   formal arguments of the handler function.

   \code{.signalSimpleWarning}, \code{.handleSimpleError}, and
   \code{.tryResumeInterrupt} are used internally and should not be
   called directly.
}

\references{The \code{tryCatch} mechanism is similar to Java
  error handling.  Calling handlers are based on Common Lisp and
  Dylan.  Restarts are based on the Common Lisp restart mechanism.}

\seealso{\code{\link{stop}} and \code{\link{warning}} signal conditions,
  and \code{\link{try}} is essentially a simplified version of
  \code{tryCatch}.
  \code{\link{assertCondition}} in package \pkg{tools} \emph{tests}
  that conditions are signalled and works with several of the above
  handlers.
}

\examples{
tryCatch(1, finally = print("Hello"))
e <- simpleError("test error")
\dontrun{
 stop(e)
 tryCatch(stop(e), finally = print("Hello"))
 tryCatch(stop("fred"), finally = print("Hello"))
}
tryCatch(stop(e), error = function(e) e, finally = print("Hello"))
tryCatch(stop("fred"),  error = function(e) e, finally = print("Hello"))
withCallingHandlers({ warning("A"); 1+2 }, warning = function(w) {})
\dontrun{
 { withRestarts(stop("A"), abort = function() {}); 1 }
}
withRestarts(invokeRestart("foo", 1, 2), foo = function(x, y) {x + y})

##--> More examples are part of
##-->   demo(error.catching)
}
\keyword{programming}
\keyword{error}
