% File src/library/base/man/Sys.time.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Sys.time}
\alias{Sys.time}
\alias{Sys.Date}

\title{Get Current Date and Time}
\description{
  \code{Sys.time} and \code{Sys.Date} returns the system's idea of the
  current date with and without time.
}
\usage{
Sys.time()
Sys.Date()
}
\details{
  \code{Sys.time} returns an absolute date-time value which can be
  converted to various time zones and may return different days.

  \code{Sys.Date} returns the current day in the current \link{time zone}.
}
\value{
  \code{Sys.time} returns an object of class \code{"POSIXct"} (see
  \link{DateTimeClasses}).  On almost all systems it will have
  sub-second accuracy, possibly microseconds or better.  On Windows it
  increments in clock ticks (usually 1/60 of a second) reported to
  millisecond accuracy.

  \code{Sys.Date} returns an object of class \code{"Date"} (see \link{Date}).
}
\note{
  \code{Sys.time} may return fractional seconds, but they are ignored by
  the default conversions (e.g., printing) for class \code{"POSIXct"}.
  See the examples and \code{\link{format.POSIXct}} for ways to reveal them.
}
\seealso{
  \code{\link{date}} for the system time in a fixed-format character
  string.

  \code{\link{Sys.timezone}}.
  
  \code{\link{system.time}} for measuring elapsed/CPU time of expressions.
}
\examples{\donttest{
Sys.time()
## print with possibly greater accuracy:
op <- options(digits.secs = 6)
Sys.time()
options(op)

## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y")

Sys.Date()
}}
\keyword{utilities}
\keyword{chron}
