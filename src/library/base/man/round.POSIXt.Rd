\name{round.POSIXt}
\alias{round.POSIXt}
\alias{trunc.POSIXt}
\title{Round / Truncate Data-Time Objects}
\description{
  Round or truncate date-time objects.
}
\usage{
\method{round}{POSIXt}(x, units=c("secs", "mins", "hours", "days"))
\method{trunc}{POSIXt}(x, units=c("secs", "mins", "hours", "days"))
}
\arguments{
  \item{x}{an object inheriting from \code{"POSIXt"}.}
  \item{units}{one of the units listed.  Can be abbreviated.}
}
\details{
  The time is rounded or truncated to the second, minute, hour or day.
  Timezones are only relevant to days, when midnight in the current
  timezone is used.
}
\value{
  An object of class \code{"POSIXlt"}.
}
\seealso{\code{\link{DateTimeClasses}}}

\examples{
round(.leap.seconds + 1000, "hour")
trunc.POSIXt(Sys.time(), "day")
}
\keyword{chron}
