\name{warning}
\alias{warning}
\alias{suppressWarnings}
\title{Warning Messages}
\description{
  Generates a warning message that corresponds to its argument(s) and
  (optionally) the expression or function from which it was called.
}
\usage{
warning(\dots, call. = TRUE, immediate. = FALSE, domain = NULL)
suppressWarnings(expr)
}
\arguments{
  \item{\dots}{character vectors (which are pasted together with no
    separator), a condition object, or \code{NULL}.}
  \item{call.}{logical, indicating if the call should become part of the
    warning message.}
  \item{immediate.}{logical, indicating if the call should be output
    immediately, even if \code{\link{getOption}(warn) = 0}.}
  \item{expr}{expression to evaluate.}
  \item{domain}{see \code{\link{gettext}}. If \code{NA}, messages will
    not be translated.}
}
\details{
  The result \emph{depends} on the value of
  \code{\link{options}("warn")} and on handlers established in the
  executing code.

  \code{warning} signals a warning condition by (effectively) calling
  \code{signalCondition}.  If there are no handlers or if all handlers
  return, then the value of \code{warn} is used to determine the
  appropriate action.  If \code{warn} is negative warnings are
  ignored; if it is zero they are stored and printed after the
  top--level function has completed; if it is one they are printed as
  they occur and if it is 2 (or larger) warnings are turned into
  errors.  Calling \code{warning(immediate. = TRUE)} turns
  \code{warn = 0} into \code{warn = 1} for this call only.

  If \code{warn} is zero (the default), a top-level variable
  \code{last.warning} is created.  It contains the warnings which can be
  printed via a call to \code{\link{warnings}}.

  Warnings will be truncated to \code{getOption("warning.length")}
  characters, default 1000.

  While the warning is being processed, a \code{muffleWarning} restart
  is available.  If this restart is invoked with \code{invokeRestart},
  then \code{warning} returns immediately.

  An attempt is made to coerce other types of inputs to \code{warning}
  to character vectors.

  \code{suppressWarnings} evaluates its expression in a context that
  ignores all warnings.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{stop}} for fatal errors,
  \code{\link{message}} for diagnostic messages,
  \code{\link{warnings}},
  and \code{\link{options}} with argument \code{warn=}.

  \code{\link{gettext}} for the mechanisms for the automated translation
  of messages.
}
\examples{
\dontshow{oldopt <- options(warn=1)}
testit <- function() warning("testit")
testit() ## shows call
testit <- function() warning("problem in testit", call. = FALSE)
testit() ## no call
suppressWarnings(warning("testit"))
\dontshow{eigenval <- 10 ^ -rnorm(1, m=6)
if(eigenval < 1.e-7) warning("system near singular")
options(oldopt)}
}
\keyword{programming}
\keyword{error}
