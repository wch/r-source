% File src/library/base/man/round.POSIXt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{round.POSIXt}
\alias{round.POSIXt}
\alias{trunc.POSIXt}
\alias{round.Date}
\alias{trunc.Date}
\title{Round / Truncate Date-Time Objects}
\description{
  Round or truncate date-time objects.
}
\usage{
\method{round}{POSIXt}(x,
      units = c("secs", "mins", "hours", "days", "months", "years"))
\method{trunc}{POSIXt}(x,
      units = c("secs", "mins", "hours", "days", "months", "years"),
      \dots)

\method{round}{Date}(x, \dots)
\method{trunc}{Date}(x,
      units = c("secs", "mins", "hours", "days", "months", "years"),
      \dots)
}
\arguments{
  \item{x}{an object inheriting from \code{"\link{POSIXt}"} or \code{"\link{Date}"}.}
  \item{units}{one of the units listed, a string.  Can be abbreviated.}
  \item{\dots}{arguments to be passed to or from other methods, notably
    \code{digits} for \code{round}.}
}
\details{
  The time is rounded or truncated to the second, minute, hour, day,
  month or year.  Time zones are only relevant to days or more, when
  midnight in the current \link{time zone} is used.

  For \code{units} arguments besides \dQuote{months} and \dQuote{years},
  the methods for class \code{"Date"} are of little use except to remove
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
\dontdiff{
         trunc(Sys.time(), "day")
(timM <- trunc(Sys.time() -> St, "months")) # shows timezone
(datM <- trunc(Sys.Date() -> Sd, "months"))
(timY <- trunc(St, "years")) # + timezone
(datY <- trunc(Sd, "years"))
}
stopifnot(inherits(datM, "Date"), inherits(timM, "POSIXt"),
          substring(format(datM), 9,10) == "01", # first of month
          substring(format(datY), 6,10) == "01-01", # Jan 1
          identical(format(datM), format(timM)),
          identical(format(datY), format(timY)))
}
\keyword{chron}
