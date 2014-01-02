% File src/library/base/man/ISOdatetime.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{ISOdatetime}
\alias{ISOdatetime}
\alias{ISOdate}
\title{Date-time Conversion Functions from Numeric Representations}
\description{
  Convenience wrappers to create date-times from numeric representations.
}
\usage{
ISOdatetime(year, month, day, hour, min, sec, tz = "")
ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")
}
\arguments{
  \item{year, month, day}{numerical values to specify a day.}
  \item{hour, min, sec}{numerical values for a time within a day.
    Fractional seconds are allowed.}
  \item{tz}{A \link{time zone} specification to be used for the conversion.
    \code{""} is the current time zone and \code{"GMT"} is UTC.}
}
\details{
  \code{ISOdatetime} and \code{ISOdate} are convenience wrappers for
  \code{strptime} that differ only in their defaults and that
  \code{ISOdate} sets UTC as the time zone.  For dates without times it
  would normally be better to use the \code{"\link{Date}"} class.

  The main arguments will be recycled using the usual recycling rules.

  Because these make use of \code{\link{strptime}}, only years in the
  range \code{0:9999} are accepted.
}
\value{
  An object of class \code{"\link{POSIXct}"}.
}
\seealso{
  \link{DateTimeClasses} for details of the date-time classes;
  \code{\link{strptime}} for conversions from character strings.
}
\keyword{utilities}
\keyword{chron}
