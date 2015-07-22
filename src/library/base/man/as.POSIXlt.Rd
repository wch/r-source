% File src/library/base/man/as.POSIXlt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{as.POSIX*}
\alias{as.POSIXct}
\alias{as.POSIXct.default}
\alias{as.POSIXct.POSIXlt}
\alias{as.POSIXct.date}
\alias{as.POSIXct.dates}
\alias{as.POSIXct.Date}
\alias{as.POSIXct.numeric}
\alias{as.POSIXlt}
\alias{as.POSIXlt.Date}
\alias{as.POSIXlt.date}
\alias{as.POSIXlt.dates}
\alias{as.POSIXlt.POSIXct}
\alias{as.POSIXlt.factor}
\alias{as.POSIXlt.character}
\alias{as.POSIXlt.default}
\alias{as.POSIXlt.numeric}
\alias{as.double.POSIXlt}

\title{Date-time Conversion Functions}
\description{
  Functions to manipulate objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times.
}
\usage{
as.POSIXct(x, tz = "", \dots)
as.POSIXlt(x, tz = "", \dots)

\method{as.POSIXlt}{character}(x, tz = "", format, \dots)

\method{as.POSIXlt}{numeric}(x, tz = "", origin, \dots)

\method{as.double}{POSIXlt}(x, \dots)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A time zone specification to be used for the conversion,
    \emph{if one is required}.  System-specific (see \link{time zones}),
    but \code{""} is the current time zone, and \code{"GMT"} is UTC
    (Universal Time, Coordinated). Invalid values are most commonly
    treated as UTC, on some platforms with a warning.}
  \item{\dots}{further arguments to be passed to or from other methods.}
  \item{format}{character string giving a date-time format as used
    by \code{\link{strptime}}.}
  \item{origin}{a date-time object, or something which can be coerced by
    \code{as.POSIXct(tz = "GMT")} to such an object.}
}
\details{
  The \code{as.POSIX*} functions convert an object to one of the two
  classes used to represent date/times (calendar dates plus time to the
  nearest second).  They can convert a wide variety of objects,
  including objects of the other class and of classes \code{"Date"},
  \code{"date"} (from package \pkg{\link[date:as.date]{date}}),
  \code{"chron"} and \code{"dates"} (from package
  \pkg{\link[chron]{chron}}) to these classes.  Dates without times are
  treated as being at midnight UTC.

  They can also convert character strings of the formats
  \code{"2001-02-03"} and \code{"2001/02/03"} optionally followed by
  white space and a time in the format \code{"14:52"} or
  \code{"14:52:03"}.  (Formats such as \code{"01/02/03"} are ambiguous
  but can be converted via a format specification by
  \code{\link{strptime}}.)  Fractional seconds are allowed.
  Alternatively, \code{format} can be specified for character vectors or
  factors: if it is not specified and no standard format works for
  all non-\code{NA} inputs an error is thrown.

  If \code{format} is specified, remember that some of the format
  specifications are locale-specific, and you may need to set the
  \code{LC_TIME} category appropriately \emph{via}
  \code{\link{Sys.setlocale}}.  This most often affects the use of
  \code{\%b}, \code{\%B} (month names) and \code{\%p} (AM/PM).

  Logical \code{NA}s can be converted to either of the classes, but no
  other logical vectors can be.

  If you are given a numeric time as the number of seconds since an
  epoch, see the examples.

  Character input is first converted to class \code{"POSIXlt"} by
  \code{\link{strptime}}: numeric input is first converted to
  \code{"POSIXct"}.  Any conversion that needs to go between the two
  date-time classes requires a time zone: conversion from
  \code{"POSIXlt"} to \code{"POSIXct"} will validate times in the
  selected time zone.  One issue is what happens at transitions
  to and from DST, for example in the UK
\preformatted{as.POSIXct(strptime("2011-03-27 01:30:00", "\%Y-\%m-\%d \%H:\%M:\%S"))
as.POSIXct(strptime("2010-10-31 01:30:00", "\%Y-\%m-\%d \%H:\%M:\%S"))
}
  are respectively invalid (the clocks went forward at 1:00 GMT to 2:00
  BST) and ambiguous (the clocks went back at 2:00 BST to 1:00 GMT).  What
  happens in such cases is OS-specific: one should expect the first to
  be \code{NA}, but the second could be interpreted as either BST or
  GMT (and common OSes give both possible values).  Note too (see
  \code{\link{strftime}}) that OS facilities may not format invalid
  times correctly.
}

\value{
  \code{as.POSIXct} and \code{as.POSIXlt} return an object of the
  appropriate class.  If \code{tz} was specified, \code{as.POSIXlt}
  will give an appropriate \code{"tzone"} attribute.  Date-times known
  to be invalid will be returned as \code{NA}.
}

\note{
  Some of the concepts used have to be extended backwards in time (the
  usage is said to be \sQuote{proleptic}).  For example, the origin of
  time for the \code{"POSIXct"} class, \sQuote{1970-01-01 00:00.00 UTC},
  is before UTC was defined.  More importantly, conversion is done
  assuming the Gregorian calendar which was introduced in 1582 and not
  used universally until the 20th century.  One of the
  re-interpretations assumed by ISO 8601:2004 is that there was a year
  zero, even though current year numbering (and zero) is a much later
  concept (525 AD for year numbers from 1 AD).

  If you want to extract specific aspects of a time (such as the day of
  the week) just convert it to class \code{"POSIXlt"} and extract the
  relevant component(s) of the list, or if you want a character
  representation (such as a named day of the week) use the
  \code{\link[base:format.POSIXlt]{format}} method.

  If a time zone is needed and that specified is invalid on your system,
  what happens is system-specific but attempts to set it will probably
  be ignored.
  
  Conversion from character needs to find a suitable format unless one
  is supplied (by trying common formats in turn): this can be slow for
  long inputs.
}
\seealso{
  \link{DateTimeClasses} for details of the classes;
  \code{\link{strptime}} for conversion to and from character
  representations.

  \code{\link{Sys.timezone}} for details of the (system-specific) naming
  of time zones.

  \link{locales} for locale-specific aspects.
}
\examples{\donttest{
(z <- Sys.time())             # the current datetime, as class "POSIXct"
unclass(z)                    # a large integer
floor(unclass(z)/86400)       # the number of days since 1970-01-01 (UTC)
(now <- as.POSIXlt(Sys.time())) # the current datetime, as class "POSIXlt"
unlist(unclass(now))          # a list shown as a named vector
now$year + 1900               # see ?DateTimeClasses
months(now); weekdays(now)    # see ?months

## suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
## (the origin used by SAS)
z <- 1472562988
# ways to convert this
as.POSIXct(z, origin = "1960-01-01")                # local
as.POSIXct(z, origin = "1960-01-01", tz = "GMT")    # in UTC

## SPSS dates (R-help 2006-02-16)
z <- c(10485849600, 10477641600, 10561104000, 10562745600)
as.Date(as.POSIXct(z, origin = "1582-10-14", tz = "GMT"))

## Stata date-times: milliseconds since 1960-01-01 00:00:00 GMT
## format %tc excludes leap-seconds, assumed here
## For format %tC including leap seconds, see foreign::read.dta()
z <- 1579598122120
op <- options(digits.secs = 3)
# avoid rounding down: milliseconds are not exactly representable
as.POSIXct((z+0.1)/1000, origin = "1960-01-01")
options(op)

## Matlab 'serial day number' (days and fractional days)
z <- 7.343736909722223e5 # 2010-08-23 16:35:00
as.POSIXct((z - 719529)*86400, origin = "1970-01-01", tz = "UTC")

as.POSIXlt(Sys.time(), "GMT") # the current time in UTC
}
\donttest{## These may not be correct names on your system
as.POSIXlt(Sys.time(), "America/New_York")  # in New York
as.POSIXlt(Sys.time(), "EST5EDT")           # alternative.
as.POSIXlt(Sys.time(), "EST" )   # somewhere in Eastern Canada
as.POSIXlt(Sys.time(), "HST")    # in Hawaii
as.POSIXlt(Sys.time(), "Australia/Darwin")
}
#ifdef windows

cols <- c("code", "coordinates", "TZ", "comments")
tmp <- read.delim(file.path(R.home("share"), "zoneinfo", "zone.tab"),
                  header = FALSE, comment.char = "#", col.names = cols)
if(interactive()) View(tmp)
#endif
}
\keyword{utilities}
\keyword{chron}
