\name{proc.time}
\alias{proc.time}
\title{Running Time of R}
\description{
  \code{proc.time} determines how much time (in seconds) the currently
  running \R process already consumed.
}
\usage{
proc.time()
}
\value{
  A numeric vector of length 5, containing the user, system, and total
  elapsed times for the currently running \R process, and the cumulative
  sum of user and system times of any child processes spawned by it.

  The resolution of the times will be system-specific; it is common for
  them to be recorded to of the order of 1/100 second, and elapsed
  time is rounded to the nearest 1/100.

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
  are genuine times on NT4/2000/XP systems.  Times of child processes are
  not available and will always be given as \code{NA}.
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
for (i in 1:50) mad(runif(500))
proc.time() - ptm
}}
\keyword{utilities}
