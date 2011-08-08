% File src/library/tools/man/psnice.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later


\name{psnice}
\alias{psnice}
\title{Get or Set the Priority (Niceness) of a Process}
\description{
  Get or set the \sQuote{niceness} of the current process, or one or
  more other processes.
}
\usage{
psnice(pid = Sys.getpid(), value = NA_integer_)
}
\arguments{
  \item{pid}{The process IDs of one of more processes: defaults to the
    \R session process.}
  \item{value}{The niceness to be set, or \code{NA} for an enquiry.}
}
\details{
  POSIX operating systems have a concept of process priorities, usually
  from 0 to 39 (or 40) with 20 being a normal priority and (somewhat
  confusingly) larger numeric values denoting lower priority.  To add to
  the confusion, there is a \sQuote{niceness} value, the amount by which
  the priority numerically exceeds 20 (which can be negative).
  Processes with high niceness will receive less CPU time than those
  with normal priority.  On some OSes, processes with niceness
  \code{+19} are only run when the system would otherwise be idle.

  On many OSes utiltiies such as \command{top} report the priority and
  not the niceness.  Niceness is used by the utility
  \file{/usr/bin/renice}: \file{/usr/bin/nice} (and
  \command{/usr/bin/renice -n}) specifies an \emph{increment} in niceness.
  
  Only privileged users (usually super-users) can lower the niceness.

  % not yet anyway.  Apparently WinNT proirities are 0-31, normal is 8,
  % and high values = high priority. Use [GS]etPriorityClass
  This function is not implemented on \code{Windows}, and may not be on
  other platforms.
}
\value{
  An integer vector of \emph{previous} niceness values,
  \code{NA} if unknown for any reason.
}

\seealso{
  \pkg{parallel} creates child processes whose priority may need to be changed.
  
  \code{\link{pskill}}.
}

\keyword{ utility }
