\name{proc.time}
\alias{proc.time}
\alias{print.proc_time}
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
  but as from \R 2.5.0 its \code{print} method prints a named vector of
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
  waited.  (The \code{print} method combines the child times with those
  of the main process.)
 
  The resolution of the times will be system-specific
#ifdef unix
  and times are rounded to the nearest 1ms.  On modern systems they will
  be that accurate, but on older systems they might be accurate to
  1/100 or 1/60 sec.
#endif
#ifdef windows
  and times are typically available to 10ms on NT-based versions of Windows.
#endif

  It is most useful for \dQuote{timing} the evaluation of \R expressions,
  which can be done conveniently with \code{\link{system.time}}.
}
\note{
#ifdef unix
  It is possible to compile \R without support for \code{proc.time},
  when the function will throw an error.
#endif
#ifdef windows
  CPU times will be returned as \code{NA} on Windows 9x/ME systems, but
  are genuine times on NT4/2000/XP/2003/Vista systems.  Times of child
  processes are not available and will always be given as \code{NA}.
#endif
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{system.time}} for timing a valid \R expression,
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
