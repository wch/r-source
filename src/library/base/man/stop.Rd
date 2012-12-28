% File src/library/base/man/stop.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{stop}
\title{Stop Function Execution}
\usage{
stop(\dots, call. = TRUE, domain = NULL)
geterrmessage()
}
\alias{stop}
\alias{geterrmessage}
\arguments{
  \item{\dots}{zero or more objects which can be coerced to character
    (and which are pasted together with no separator) or a single
    condition object.}
  \item{call.}{logical, indicating if the call should become part of the
    error message.}
  \item{domain}{see \code{\link{gettext}}.  If \code{NA}, messages will
    not be translated.}
}
\description{
  \code{stop} stops execution of the current expression and executes
  an error action.

  \code{geterrmessage} gives the last error message.
}
\details{
  The error action is controlled by error handlers established within
  the executing code and by the current default error handler set by
  \code{options(error=)}.  The error is first signaled as if using
  \code{\link{signalCondition}()}.  If there are no handlers or if all handlers
  return, then the error message is printed (if
  \code{options("show.error.messages")} is true) and the default error
  handler is used.  The default behaviour (the \code{NULL}
  error-handler) in interactive use is to return to the top level
  prompt or the top level browser, and in non-interactive use to
  (effectively) call \code{\link{q}("no", status = 1, runLast = FALSE}).
  The default handler stores the error message in a buffer; it can be
  retrieved by \code{geterrmessage()}.  It also stores a trace of
  the call stack that can be retrieved by \code{\link{traceback}()}.

  Errors will be truncated to \code{getOption("warning.length")}
  characters, default 1000.

  If a condition object is supplied it should be the only argument, and
  further arguments will be ignored, with a warning.
}
\note{
  Use \code{domain = NA} whenever \code{\dots} contain a
  result from \code{\link{gettextf}()} as that is translated already.
}
\value{
  \code{geterrmessage} gives the last error message, as a character string
  ending in \code{"\\n"}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{warning}}, \code{\link{try}} to catch errors and retry,
  and \code{\link{options}} for setting error handlers.
  \code{\link{stopifnot}} for validity testing.  \code{tryCatch}
  and \code{withCallingHandlers} can be used to establish custom handlers
  while executing an expression.

  \code{\link{gettext}} for the mechanisms for the automated translation
  of messages.
}
\examples{
options(error = expression(NULL))
# don't stop on stop(.)  << Use with CARE! >>

iter <- 12
if(iter > 10) stop("too many iterations")

tst1 <- function(...) stop("dummy error")
tst1(1:10, long, calling, expression)

tst2 <- function(...) stop("dummy error", call. = FALSE)
tst2(1:10, longcalling, expression, but.not.seen.in.Error)

options(error = NULL)  # restore to default
}
\keyword{environment}
\keyword{programming}
\keyword{error}
