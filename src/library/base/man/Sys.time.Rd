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
  \link{DateTimeClasses}).

  \code{Sys.Date} returns an object of class \code{"Date"} (see \link{Date}).

  \code{Sys.timezone} returns an OS-specific character string, possibly
  an empty string.
}
\seealso{
  \code{\link{date}} for the system time in a fixed-format character
  string; \code{\link{proc.time}} for finer resolution.
}
\examples{
Sys.time()
## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y")

Sys.Date()

Sys.timezone()
}
\keyword{utilities}
\keyword{chron}
