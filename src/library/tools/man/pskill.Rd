% File src/library/tools/man/pskill.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{pskill}
\alias{pskill}

\alias{SIGHUP}
\alias{SIGINT}
\alias{SIGQUIT}
\alias{SIGKILL}
\alias{SIGTERM}
\alias{SIGSTOP}
\alias{SIGTSTP}
\alias{SIGCONT}
\alias{SIGCHLD}
\alias{SIGUSR1}
\alias{SIGUSR2}

\title{Kill a Process}
\description{
  \code{pskill} sends a signal to a process, usually to terminate it.
}
\usage{
pskill(pid, signal = SIGTERM)

SIGHUP
SIGINT
SIGQUIT
SIGKILL
SIGTERM
SIGSTOP
SIGTSTP
SIGCHLD
SIGUSR1
SIGUSR2
}
\arguments{
  \item{pid}{positive integers: one or more process IDs as returned by
    \code{\link{Sys.getpid}}.}
  \item{signal}{integer, most often one of the symbolic constants.}
}
\details{
  Signals are a C99 concept, but only a small number are required to be
  supported (of those listed, only \code{SIGINT} and \code{SIGTERM}).
  They are much more widely used on POSIX operating systems (which
  should define all of those listed here), which also support a
  \code{kill} system call to send a signal to a process, most often to
  terminate it.  Function \code{pskill} provides a wrapper: it silently
  ignores invalid values of its arguments, including zero or negative pids.

  In normal use on a Unix-alike, \kbd{Ctrl-C} sends \code{SIGINT},
  \kbd{Ctrl-\\} sends \code{SIGQUIT} and \kbd{Ctrl-Z} sends
  \code{SIGTSTP}: that and \code{SIGSTOP} suspend a process which can be
  resumed by \code{SIGCONT}.

  The signals are small integers, but the actual numeric values are not
  standardized (and most do differ between OSes).  The \code{SIG*}
  objects contain the appropriate integer values for the current platform
  (or \code{NA_INTEGER_} if the signal is not defined).

  Only \code{SIGINT} and \code{SIGKILL} will be defined on Windows, and
  \code{pskill} will always use the Windows system call
  \code{TerminateProcess}.
}
\value{
  A logical vector of the same length as \code{pid},
  \code{TRUE} (for success) or \code{FALSE}, invisibly.
}
\seealso{
  Package \pkg{parallel} has several means to launch child processes
  which record the process IDs.

  \code{\link{psnice}}
}

\examples{\dontrun{
pskill(c(237, 245), SIGKILL)
}}
\keyword{utilities}
