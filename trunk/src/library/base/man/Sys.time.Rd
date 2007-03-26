\name{Sys.time}
\alias{Sys.time}
\alias{Sys.Date}
\alias{Sys.timezone}

\title{Get Current Date, Time and Timezone}
\description{
  \code{Sys.time} and \code{Sys.Date} returns the system's idea of the
  current date with and without time, and \code{Sys.timezone} returns
  the current time zone.
}
\usage{
Sys.time()
Sys.Date()
Sys.timezone()
}
\details{
  \code{Sys.time} returns an absolute date-time value which can be
  converted in various time zones and may return different days.

  \code{Sys.Date} returns the day in the current timezone.
}
\value{
  \code{Sys.time} returns an object of class \code{"POSIXct"} (see
  \link{DateTimeClasses}).  On some systems it will have sub-second
  accuracy, but on others it will increment in seconds.
#ifdef unix
  On systems conforming to POSIX 1003.1-2001 the time will be reported
  in microsecond increments.
#endif
#ifdef windows
  On Windows, it increments in clock ticks (1/60 of a second) reported
  to millisecond accuracy.
#endif

  \code{Sys.Date} returns an object of class \code{"Date"} (see \link{Date}).

  \code{Sys.timezone} returns an OS-specific character string, possibly
  an empty string.  It may be possible to set the timezone via the
  environment variable \code{"TZ"}: see \code{\link{as.POSIXlt}}.
#ifdef windows
  Windows is notorious for naming its timezones differently from the
  official names.
#endif
}
\seealso{
  \code{\link{date}} for the system time in a fixed-format character
  string; the elapsed time component of \code{\link{proc.time}} 
  for possibly finer resolution in changes in time.
}
\examples{
Sys.time()
## print with possibly greater accuracy:
op <- options(digits.secs=6)
Sys.time()
options(op)

## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y")

Sys.Date()

Sys.timezone()
}
\keyword{utilities}
\keyword{chron}
