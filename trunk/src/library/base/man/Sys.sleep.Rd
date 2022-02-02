% File src/library/base/man/Sys.sleep.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{Sys.sleep}
\alias{Sys.sleep}
\title{Suspend Execution for a Time Interval}
\description{
  Suspend execution of \R expressions for a specified time interval.
}
\usage{
Sys.sleep(time)
}
\arguments{
  \item{time}{The time interval to suspend execution for, in seconds.}
}
\details{
  Using this function allows \R to temporarily be given very low
  priority and hence not to interfere with more important foreground
  tasks.  A typical use is to allow a process launched from \R to set
  itself up and read its input files before \R execution is resumed.

  The intention is that this function suspends execution of \R
  expressions but wakes the process up often enough to respond to GUI
  events, typically every half second.  It can be interrupted
  (e.g.\sspace{}by \samp{Ctrl-C} or \samp{Esc} at the \R console).

  There is no guarantee that the process will sleep for the whole of the
  specified interval (sleep might be interrupted), and it may well take
  slightly longer in real time to resume execution.

  \code{time} must be non-negative (and not \code{NA} nor \code{NaN}):
  \code{Inf} is allowed (and might be appropriate if the intention is to
  wait indefinitely for an interrupt).  The resolution of the time
  interval is system-dependent, but will normally be 20ms or better.
  (On modern Unix-alikes it will be better than 1ms.)
}
\value{
  Invisible \code{NULL}.
}
\note{
  Despite its name, this is not currently implemented using the
  \code{sleep} system call (although on Windows it does make use of
  \code{Sleep}).
}

\examples{\donttest{
testit <- function(x)
{
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
}
testit(3.7)
}}
\keyword{utilities}
