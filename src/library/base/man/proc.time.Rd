\name{proc.time}
\title{Running Time of R}
\usage{
proc.time()
}
\alias{proc.time}
\description{
  \code{proc.time} determines how much time (in seconds) the currently
  running \R process already consumed.
}
\value{
  A numeric vector of length 5, containing the user, system, and total
  elapsed times for the currently running \R process, and the cumulative
  sum of user and system times of any child processes spawned by it.

  The resolution of the times will be system-specific; it is common for
  them to be recorded to of the order of 1/100 second, and elapsed
  time is rounded to the nearest 1/100.

  It is most useful for ``timing'' the evaluation of \R expressions,
  which can be done conveniently with \code{\link{system.time}}.
}
#ifdef unix
\note{
  It is possible to compile \R without support for \code{proc.time},
  when the function will not exist.
}
#endif
#ifdef windows
\note{
  CPU times will be returned as \code{NA} on Windows 9x systems, but
  are genuine times on NT4 and 2000 systems. Times of child processes are
  not available and will always be given as \code{NA}.
}
#endif
\seealso{
  \code{\link{system.time}} for timing a valid \R expression.
}
\examples{
ptm <- proc.time()
for (i in 1:50) mad(runif(500))
proc.time() - ptm
}
\keyword{utilities}
