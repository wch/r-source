\name{system.time}
\title{CPU Time Used}
\usage{
system.time(expr, gcFirst)
unix.time(expr, gcFirst)
}
\alias{system.time}
\alias{unix.time}
\arguments{
  \item{expr}{Valid \R expression to be \dQuote{timed}}
  \item{gcFirst}{Logical - should a garbage collection be performed
    immediately before the timing?  Default is \code{FALSE}.}
}
\description{Return CPU (and other) times that \code{expr} used.
}    
\details{
  \code{system.time} calls the builtin \code{\link{proc.time}},
  evaluates \code{expr}, and then calls \code{proc.time} once more,
  returning the difference between the two \code{proc.time} calls.

  The values returned by the \code{proc.time} are (on Unix) those
  returned by the C library function \code{times}(3v), if available.

  \code{unix.time} is an alias of \code{system.time}, for
  compatibility reasons.

  Timings of evaluations of the same expression can vary considerably
  depending on whether the evaluation triggers a garbage collection. When
  \code{gcFirst} is \code{TRUE} a garbage collection (\code{\link{gc}})
  will be performed immediately before the evaluation of \code{expr}.
  This will usually produce more consistent timings.
}
\value{
  A numeric vector of length 5 containing the user cpu, system cpu, elapsed,
  subproc1, subproc2 times. The subproc times are the user and
  system cpu time used by child processes (and so are usually zero).
#ifdef windows
  On Windows the subproc times are not available and so are always
  \code{NA}. The first two components are not available on Windows 9x,
  and so are reported as \code{NA}; they do return real values on
  Windows NT4/2000/XP.
#endif

  The resolution of the times will be system-specific; it is common for
  them to be recorded to of the order of 1/100 second, and elapsed
  time is rounded to the nearest 1/100.
}
#ifdef unix
\note{
  It is possible to compile \R without support for \code{system.time},
  when all the values will be \code{NA}.
}
#endif
\seealso{
  \code{\link{proc.time}}, \code{\link{time}} which is for time series.
}
\examples{
require(stats)
system.time(for(i in 1:100) mad(runif(1000)))
\dontrun{
exT <- function(n = 1000) {
  # Purpose: Test if system.time works ok;   n: loop size
  system.time(for(i in 1:n) x <- mean(rt(1000, df=4)))
}
#-- Try to interrupt one of the following (using Ctrl-C / Escape):
exT()                 #- about 3 secs on a 1GHz PIII
system.time(exT())    #~ +/- same
}}
\keyword{utilities}
