% File src/library/tools/man/psnice.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Team
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
  \item{pid}{positive integers: the process IDs of one of more
    processes: defaults to the \R session process.}
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

  On many OSes utilities such as \command{top} report the priority and
  not the niceness.  Niceness is used by the utility
  \file{/usr/bin/renice}: \file{/usr/bin/nice} (and
  \command{/usr/bin/renice -n}) specifies an \emph{increment} in niceness.

  Only privileged users (usually super-users) can lower the niceness.

  Windows has a slightly different concept of \sQuote{priority
    classes}.  We have mapped the idle priority to niceness \code{19},
  \sQuote{below normal} to \code{15}, normal to \code{0}, \sQuote{above
    normal} to \code{-5} and \sQuote{realtime} to \code{-10}.  Unlike
  Unix-alikes, a non-privileged user can increase the priority class on
  Windows (but using \sQuote{realtime} is inadvisable).
}
\value{
  An integer vector of \emph{previous} niceness values,
  \code{NA} if unknown for any reason.
}

\seealso{
  Various functions in package \pkg{parallel} create child processes
  whose priority may need to be changed.

  \code{\link{pskill}}.
}

\keyword{ utility }
