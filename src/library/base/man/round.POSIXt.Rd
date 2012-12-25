% File src/library/base/man/round.POSIXt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{round.POSIXt}
\alias{round.POSIXt}
\alias{trunc.POSIXt}
\alias{round.Date}
\alias{trunc.Date}
\title{Round / Truncate Data-Time Objects}
\description{
  Round or truncate date-time objects.
}
\usage{
\method{round}{POSIXt}(x, units = c("secs", "mins", "hours", "days"))
\method{trunc}{POSIXt}(x, units = c("secs", "mins", "hours", "days"), \dots)

\method{round}{Date}(x, \dots)
\method{trunc}{Date}(x, \dots)
}
\arguments{
  \item{x}{an object inheriting from \code{"POSIXt"} or \code{"Date"}.}
  \item{units}{one of the units listed.  Can be abbreviated.}
  \item{\dots}{arguments to be passed to or from other methods, notably
    \code{digits} for \code{round}.}
}
\details{
  The time is rounded or truncated to the second, minute, hour or day.
  Timezones are only relevant to days, when midnight in the current
  \link{timezone} is used.

  The methods for class \code{"Date"} are of little use except to remove
  fractional days.
}
\value{
  An object of class \code{"POSIXlt"} or \code{"Date"}.
}
\seealso{
  \code{\link{round}} for the generic function and default methods.

  \code{\link{DateTimeClasses}}, \code{\link{Date}}
}

\examples{
round(.leap.seconds + 1000, "hour")
\donttest{trunc(Sys.time(), "day")}
}
\keyword{chron}
