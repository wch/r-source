% File src/library/base/man/quit.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{quit}
\alias{quit}
\alias{q}
\alias{.Last}
\alias{.Last.sys}
\title{Terminate an R Session}
\usage{
quit(save = "default", status = 0, runLast = TRUE)
   q(save = "default", status = 0, runLast = TRUE)
}
\arguments{
    \item{save}{a character string indicating whether the environment
        (workspace) should be saved, one of \code{"no"}, \code{"yes"},
        \code{"ask"} or \code{"default"}.}
    \item{status}{the (numerical) error status to be returned to the
        operating system, where relevant.  Conventionally \code{0}
        indicates successful completion.}
    \item{runLast}{should \code{.Last()} be executed?}
}
\description{
    The function \code{quit} or its alias \code{q} terminate the current
    \R session.
}
\details{
    \code{save} must be one of \code{"no"}, \code{"yes"},
    \code{"ask"} or \code{"default"}.  In the first case the workspace
    is not saved, in the second it is saved and in the third the user is
    prompted and can also decide \emph{not} to quit.  The default is to
    ask in interactive use but may be overridden by command-line
    arguments (which must be supplied in non-interactive use).

    Immediately \emph{before} terminating, \code{.Last()} is executed if
    the function \code{.Last} exists and \code{runLast} is true.  If in
    interactive use there are errors in the \code{.Last} function,
    control will be returned to the command prompt, so do test the
    function thoroughly.  There is a system analogue,
    \code{.Last.sys()}, which is run after \code{.Last()} if
    \code{runLast} is true.

    Exactly what happens at termination of an \R session depends on the
    platform and GUI interface in use.  A typical sequence is to run
    \code{.Last()} and \code{.Last.sys()} (unless \code{runLast} is
    false), to save the workspace if requested (and in most cases also
    to save the session history: see \code{\link{savehistory}}), then
    run any finalizers (see \code{\link{reg.finalizer}}) that have been
    set to be run on exit, close all open graphics devices, remove the
    session temporary directory and print any remaining warnings
    (e.g. from \code{.Last()} and device closure).

    Some error statuses are used by \R itself.  The default error
    handler for non-interactive use effectively calls \code{q("no", 1,
      FALSE)} and returns error code 1.  Error status 2 is used for \R
    \sQuote{suicide}, that is a catastrophic failure, and other small
    numbers are used by specific ports for initialization failures.  It
    is recommended that users choose statuses of 10 or more.

    Valid values of \code{status} are system-dependent, but \code{0:255}
    are normally valid.  (Many OSes will report the last byte of the
    value, that is report the number modulo 256.  But not all.)
#ifdef windows
% https://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/ntcmds_shelloverview.mspx?mfr=true
% https://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/if.mspx?mfr=true

    Windows calls the status the \sQuote{error code} or \sQuote{exit
      code}.  It is returned in the environment variable
    \env{\%ERRORLEVEL\%} in \code{cmd.exe}, and in \env{LASTEXITCODE}
    in Windows PowerShell.  Note that the \code{Rterm} reliably reports
    the \code{status} value, but \code{Rgui} may give an error code from
    the GUI interface.
#endif
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\note{
  The \code{R.app} GUI on OS X has its own version of these functions
  with slightly different behaviour for the \code{save} argument (the
  \sQuote{Startup} preferences for this action are taken into account).
}
\seealso{
  \code{\link{.First}} for setting things on startup.
}
\examples{
\dontrun{## Unix-flavour example
.Last <- function() {
  cat("Now sending PDF graphics to the printer:\n")
  system("lpr Rplots.pdf")
  cat("bye bye...\n")
}
quit("yes")}
}
\keyword{environment}
