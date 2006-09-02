\name{as.POSIX*}
\alias{as.POSIXct}
\alias{as.POSIXct.default}
\alias{as.POSIXct.POSIXlt}
\alias{as.POSIXct.date}
\alias{as.POSIXct.dates}
\alias{as.POSIXct.Date}
\alias{as.POSIXlt}
\alias{as.numeric.POSIXlt}

\title{Date-time Conversion Functions}
\description{
  Functions to manipulate objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times.
}
\usage{
as.POSIXct(x, tz = "")
as.POSIXlt(x, tz = "")

\method{as.numeric}{POSIXlt}(x)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A timezone specification to be used for the conversion,
    \emph{if one is required}.  System-specific, but \code{""}
    is the current timezone, and \code{"GMT"} is UTC
    (Coordinated Universal Time, in French).}
}
\details{
  The \code{as.POSIX*} functions convert an object to one of the two
  classes used to represent date/times (calendar dates plus time to the
  nearest second).  They can convert a wide variety of objects,
  including objects of the other class and of classes \code{"Date"},
  \code{"date"} (from package \pkg{\link[date:as.date]{date}} or
  \pkg{\link[survival:as.date]{survival}}), \code{"chron"} and
  \code{"dates"} (from package \pkg{\link[chron]{chron}}) to these
  classes.  Dates without times are treated as being at midnight UTC.

  They can also convert character strings of the formats
  \code{"2001-02-03"} and \code{"2001/02/03"} optionally followed by
  white space and a time in the format \code{"14:52"} or
  \code{"14:52:03"}.  (Formats such as \code{"01/02/03"} are ambiguous
  but can be converted via a format specification by
  \code{\link{strptime}}.)  (As from \R 2.4.0 fractional seconds can be
  converted.)

  Logical \code{NA}s can be converted to either of the classes, but no
  other logical vectors can be.

  The \code{as.numeric} method converts \code{"POSIXlt"} objects to
  \code{"POSIXct"}.

  If you are given a numeric time as the number of seconds since an
  epoch, see the examples.

  Where OSes describe their valid timezones can be obscure.  The help
  for \code{tzset} (or \code{_tzset} on Windows) can be helpful, but it
  can also be inaccurate.  There is a cumbersome POSIX specification,
  (listed under environment variable \code{TZ} at
  \url{http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html}),
  which is often at least partially supported, but there may be other more
  user-friendly ways to specify timezones.
#ifdef unix
  For most Unix-alikes (including MacOS X) this can be an optional colon
  prepended to the path to a file (by default under
  \file{/usr/share/zoneinfo} or \file{/usr/lib/zoneinfo}
  (or even \file{/usr/share/lib/zoneinfo} on Solaris)),
  for example \file{EST5EDT} or \file{GB} or \file{Europe/Paris}.  See
  \url{http://www.twinsun.com/tz/tz-link.htm} for more details and
  references.
#endif
#ifdef windows
  Windows documents a specification of the form \code{GST-1GDT}, but
  seems always to apply the US rules for changing to/from DST with such a
  specification.  (There appears to be no documented way to apply
  timezones using other DST rules except to switch to the timezone,
  e.g. from the Control Panel.)  So valid time zone values are of the
  forms \code{EST5EDT} for a timezone 5 hours west of GMT called
  \code{EST} when daylight saving is in force and \code{EDT} when it is
  not, \code{HST10} for \sQuote{Hawaii-Aleutian Standard Time} (which has no
  DST), and \code{CST-9:30} for the Northern Territory of Australia.
  The abbreviations have to be exactly 3 letters, and have no meaning
  other than as labels on the output.
#endif
}
\value{
  \code{as.POSIXct} and \code{as.POSIXlt} return an object of the
  appropriate class.  If \code{tz} was specified, \code{as.POSIXlt}
  will give an appropriate \code{"tzone"} attribute.
}
\note{
  If you want to extract specific aspects of a time (such as the day of
  the week) just convert it to class \code{"POSIXlt"} and extract the
  relevant component(s) of the list, or if you want a character
  representation (such as a named day of the week) use
  \code{\link{format.POSIXlt}} or \code{\link{format.POSIXct}}.

  If a timezone is needed and that specified is invalid on your system,
  what happens is system-specific but it will probably be ignored.
}
\seealso{
  \link{DateTimeClasses} for details of the classes;
  \code{\link{strptime}} for conversion to and from character
  representations.
}
\examples{
(z <- Sys.time())             # the current datetime, as class "POSIXct"
unclass(z)                    # a large integer
floor(unclass(z)/86400)       # the number of days since 1970-01-01
(z <- as.POSIXlt(Sys.time())) # the current datetime, as class "POSIXlt"
unlist(unclass(z))            # a list shown as a named vector

## suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
z <- 1472562988
# two ways to convert this
ISOdatetime(1960,1,1,0,0,0) + z # late August 2006
strptime("1960-01-01", "\%Y-\%m-\%d", tz="GMT") + z

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
#ifdef windows
as.POSIXlt(Sys.time(), "EST5EDT")  # the current time in New York
as.POSIXlt(Sys.time(), "HST10")    # the current time in Hawaii
as.POSIXlt(Sys.time(), "CST-9:30") # the current time in Darwin
#endif
#ifdef unix
\dontrun{## These may not be correct names on your system
as.POSIXlt(Sys.time(), "EST5EDT")  # the current time in New York
as.POSIXlt(Sys.time(), "EST" )     # ditto, ignoring DST
as.POSIXlt(Sys.time(), "HST")      # the current time in Hawaii
as.POSIXlt(Sys.time(), "Australia/Darwin")
}
#endif
}
\keyword{utilities}
\keyword{chron}
