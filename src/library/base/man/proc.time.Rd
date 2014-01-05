% File src/library/base/man/proc.time.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{proc.time}
\alias{proc.time}
\alias{print.proc_time}
\alias{summary.proc_time}
\title{Running Time of R}
\description{
  \code{proc.time} determines how much real and CPU time (in seconds)
  the currently running \R process has already taken.
}
\usage{
proc.time()
}
\details{
  \code{proc.time} returns five elements for backwards compatibility,
  but its \code{print} method prints a named vector of
  length 3.  The first two entries are the total user and system CPU
  times of the current \R process and any child processes on which it
  has waited, and the third entry is the \sQuote{real} elapsed time
  since the process was started.
}
\value{
  An object of class \code{"proc_time"} which is a numeric vector of
  length 5, containing the user, system, and total elapsed times for the
  currently running \R process, and the cumulative sum of user and
  system times of any child processes spawned by it on which it has
  waited.  (The \code{print} method uses the \code{summary} method to
  combine the child times with those of the main process.)
  
  The definition of \sQuote{user} and \sQuote{system} times is from your
  OS.  Typically it is something like

  \emph{The \sQuote{user time} is the CPU time charged for the execution
  of user instructions of the calling process.  The \sQuote{system time}
  is the CPU time charged for execution by the system on behalf of the
  calling process.}

  Times of child processes are not available on Windows and will always
  be given as \code{NA}.

  The resolution of the times will be system-specific and on Unix-alikes
  times are rounded down to milliseconds.  On modern systems they will
  be that accurate, but on older systems they might be accurate to 1/100
  or 1/60 sec.  They are typically available to 10ms on Windows.

  This is a \link{primitive} function.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{system.time}} for timing an \R expression,
  \code{\link{gc.time}} for how much of the time was spent in garbage
  collection.
}
\examples{\dontrun{
## a way to time an R expression: system.time is preferred
ptm <- proc.time()
for (i in 1:50) mad(stats::runif(500))
proc.time() - ptm
}}
\keyword{utilities}
