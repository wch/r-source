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

  It is most useful for ``timing'' the evaluation of \R expressions,
  which can be done conveniently with \code{\link{system.time}}.
}
\seealso{
  \code{\link{system.time}} for timing a valid \R expression
}
\examples{
ptm <- proc.time()
for (i in 1:50) mad(runif(500))
proc.time() - ptm
}
\keyword{utilities}
