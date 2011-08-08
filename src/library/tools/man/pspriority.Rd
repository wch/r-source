% File src/library/tools/man/psnice.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later


\name{psnice}
\alias{psnice}
\title{Get or Set the Priority (Niceness) of a Process}
}
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
  from 0 to 39 with 20 being a normal priority and (somewhat
  confusingly) larger numeric values denoting lower priority.  To add to
  the confusion, there is a \sQuote{niceness} value, the amount by which
  the priority numerically exceeds 20 (which can be negative).
  Processes with high niceness will receive less CPU time than those
  with normal priority.  On some OSes, processes with niceness
  \code{+19} are only run when the system would otherwise be idle.
  
  Only privileged users (usually super-users) can lower the niceness.
  
  This function is not implemented on \code{Windows}.
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
