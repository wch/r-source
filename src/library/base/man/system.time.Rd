\name{system.time}
\title{CPU Time Used}
\usage{
system.time(expr)
}
\alias{system.time}
\arguments{
  \item{expr}{Valid \R expression to be ``timed''}
}
\description{
  \code{system.time} calls the builtin \code{\link{proc.time}},
  evaluates \code{expr}, and the calls \code{proc.time} once more,
  returning the difference to between the \code{proc.time} calls.

  The values returned by the \code{proc.time} are (currently) those
  returned by the C library function times(3v).
}
\value{
  A numeric vector of length 5 containing the cpu, user, total,
  subproc1, subproc2 times.
}
\seealso{
  \code{\link{proc.time}}, \code{\link{time}} which is for time series.
}
\examples{
system.time(for(i in 1:50) mad(runif(500)))
exT <- function(n = 100) {
  # Purpose: Test if system.time works ok;   n: loop size
  system.time(for(i in 1:n) x <- mean(rt(1000, df=4)))
}
#-- Try to interrupt one of the following (using Ctrl-C):
exT()                 #- `1.4' on -O-optimized Ultra1
system.time(exT())    #~ +/- same
}
\keyword{utilities}
