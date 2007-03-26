\name{system.time}
\title{CPU Time Used}
\usage{
system.time(expr, gcFirst = TRUE)
unix.time(expr, gcFirst = TRUE)
}
\alias{system.time}
\alias{unix.time}
\arguments{
  \item{expr}{Valid \R expression to be \dQuote{timed}}
  \item{gcFirst}{Logical - should a garbage collection be performed
    immediately before the timing?  Default is \code{TRUE}.}
}
\description{Return CPU (and other) times that \code{expr} used.
}    
\details{
  \code{system.time} calls the function \code{\link{proc.time}},
  evaluates \code{expr}, and then calls \code{proc.time} once more,
  returning the difference between the two \code{proc.time} calls.

  \code{unix.time} is an alias of \code{system.time}, for
  compatibility with S.

  Timings of evaluations of the same expression can vary considerably
  depending on whether the evaluation triggers a garbage collection. When
  \code{gcFirst} is \code{TRUE} a garbage collection (\code{\link{gc}})
  will be performed immediately before the evaluation of \code{expr}.
  This will usually produce more consistent timings.
}
\value{
  A object of class \code{"proc_time"}: see
  \code{\link{proc.time}} for details.
}
#ifdef unix
\note{
  It is possible to compile \R without support for \code{system.time},
  when the function will throw an error.
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
