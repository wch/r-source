% File src/library/base/man/unix/Signals.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Signals}
\alias{Signals}
\title{Interrupting Execution of R}
\description{
  On receiving \code{SIGUSR1} \R will save the workspace and quit.
  \code{SIGUSR2} has the same result except that the \code{\link{.Last}}
  function and \code{\link{on.exit}} expressions will not be called.
}
\usage{
\special{kill -USR1 pid}
\special{kill -USR2 pid}
}
\arguments{
  \item{pid}{The process ID of the \R process.}
}
\details{
  The commands history will also be saved if would be at normal
  termination.

  This is not available on Windows, and possibly on other OSes which do
  not support these signals.
}
\section{Warning}{
  It is possible that one or more \R objects will be undergoing
  modification at the time the signal is sent.  These objects could be
  saved in a corrupted form.
}
\seealso{
  \code{\link{Sys.getpid}} to report the process ID for future use.
}
\keyword{utilities}
