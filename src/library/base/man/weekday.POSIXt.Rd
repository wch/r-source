% File src/library/base/man/weekday.POSIXt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{weekdays}
\alias{weekdays}
\alias{weekdays.POSIXt}
\alias{weekdays.Date}
\alias{months}
\alias{months.POSIXt}
\alias{months.Date}
\alias{quarters}
\alias{quarters.POSIXt}
\alias{quarters.Date}
\alias{julian}
\alias{julian.POSIXt}
\alias{julian.Date}
\title{Extract Parts of a POSIXt or Date Object}
\description{
  Extract the weekday, month or quarter, or the Julian time
  (days since some origin).  These are generic functions: the methods
  for the internal date-time classes are documented here.
}
\usage{
weekdays(x, abbreviate)
\method{weekdays}{POSIXt}(x, abbreviate = FALSE)
\method{weekdays}{Date}(x, abbreviate = FALSE)

months(x, abbreviate)
\method{months}{POSIXt}(x, abbreviate = FALSE)
\method{months}{Date}(x, abbreviate = FALSE)

quarters(x, abbreviate)
\method{quarters}{POSIXt}(x, \dots)
\method{quarters}{Date}(x, \dots)

julian(x, \dots)
\method{julian}{POSIXt}(x, origin = as.POSIXct("1970-01-01", tz = "GMT"), \dots)
\method{julian}{Date}(x, origin = as.Date("1970-01-01"), \dots)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"} or \code{"Date"}.}
  \item{abbreviate}{logical.  Should the names be abbreviated?}
  \item{origin}{an length-one object inheriting from class
    \code{"POSIXt"} or \code{"Date"}.}
  \item{\dots}{arguments for other methods.}
}
\value{
  \code{weekdays} and \code{months} return a character
  vector of names in the locale in use.

  \code{quarters} returns a character vector of \code{"Q1"} to
  \code{"Q4"}.

  \code{julian} returns the number of days (possibly fractional)
  since the origin, with the origin as a \code{"origin"} attribute.
  All time calculations in \R are done ignoring leap-seconds.
}
\note{
  Other components such as the day of the month or the year are
  very easy to compute: just use \code{\link{as.POSIXlt}} and extract
  the relevant component.  Alternatively (especially if the components
  are desired as character strings), use \code{\link{strftime}}.
}

\seealso{
  \code{\link{DateTimeClasses}}, \code{\link{Date}}
}

\examples{\donttest{
weekdays(.leap.seconds)
months(.leap.seconds)
quarters(.leap.seconds)

## Julian Day Number (JDN, https://en.wikipedia.org/wiki/Julian_day)
## is the number of days since noon UTC on the first day of 4317 BC.
## in the proleptic Julian calendar.  To more recently, in
## 'Terrestrial Time' which differs from UTC by a few seconds
## See https://en.wikipedia.org/wiki/Terrestrial_Time
julian(Sys.Date(), -2440588) # from a day
floor(as.numeric(julian(Sys.time())) + 2440587.5) # from a date-time
}}
\keyword{chron}
