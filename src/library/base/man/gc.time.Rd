\name{gc.time}
\alias{gc.time}
\title{Report Time Spent in Garbage Collection}
\description{
  This function reports the time spent in garbage collection so far in
  the \R session while GC timing was enabled..
}
\usage{
gc.time(on = TRUE)
}
\arguments{
  \item{on}{logical; if \code{TRUE}, GC timing is enabled.}
}
\value{
  A numerical vector of length 5 giving the user CPU time, the system
  CPU time, the elapsed time and children's user and system CPU times
  (normally both zero).
}
#ifdef windows
\note{
  CPU times will be returned as \code{NA} on Windows 9x/ME systems, but
  are genuine times on NT4/2000/XP/2003/Vista systems.  Times of child
  processes are not available and will always be given as \code{NA}.
}
#endif
\section{Warnings}{
  This is experimental functionality, likely to be removed as soon as
  the next release.

  The timings are rounded up by the sampling interval for timing
  processes, and so are likely to be over-estimates.
}
\seealso{\code{\link{gc}},
  \code{\link{proc.time}} for the timings for the session.}

\examples{
gc.time()
}
\keyword{utilities}
