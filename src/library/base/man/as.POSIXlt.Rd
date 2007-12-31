% File src/library/base/man/as.POSIXlt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
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
  \item{tz}{A timezone specification to be used for the conversion,
    \emph{if one is required}.  System-specific (see \link{time zones}),
    but \code{""} is the current timezone, and \code{"GMT"} is UTC
    (Universal Time, Coordinated).}
  \item{\dots}{further arguments to be passed to or from other methods.}
  \item{format}{character string giving a date-time format as used
    by \code{\link{strptime}}.}
  \item{origin}{a date-time object, or something which can be coerced by
    \code{as.POSIXct(tz="GMT")} to such an object.}
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
  \code{\link{strptime}}.)  Fractional seconds are allowed.
  Alternatively, \code{format} can be specified for character vectors or
  factors.

  Logical \code{NA}s can be converted to either of the classes, but no
  other logical vectors can be.

  The \code{as.double} method converts \code{"POSIXlt"} objects to
  \code{"POSIXct"}.

  If you are given a numeric time as the number of seconds since an
  epoch, see the examples.
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
  representations. \code{\link{Sys.timezone}} for details of the
  (system-specific)naming of time zones.
}
\examples{
(z <- Sys.time())             # the current datetime, as class "POSIXct"
unclass(z)                    # a large integer
floor(unclass(z)/86400)       # the number of days since 1970-01-01
(z <- as.POSIXlt(Sys.time())) # the current datetime, as class "POSIXlt"
unlist(unclass(z))            # a list shown as a named vector

## suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
z <- 1472562988
# ways to convert this
as.POSIXct(z, origin="1960-01-01")                # local
as.POSIXct(z, origin="1960-01-01", tz="GMT")      # in UTC
as.POSIXct(z, origin=ISOdatetime(1960,1,1,0,0,0)) # local
ISOdatetime(1960,1,1,0,0,0) + z                   # local

## SPSS dates (R-help 2006-02-17)
z <- c(10485849600, 10477641600, 10561104000, 10562745600)
as.Date(as.POSIXct(z, origin="1582-10-14", tz="GMT"))

as.POSIXlt(Sys.time(), "GMT") # the current time in UTC
\dontrun{## These may not be correct names on your system
as.POSIXlt(Sys.time(), "America/New_York")  # in New York
as.POSIXlt(Sys.time(), "EST5EDT")           # alternative.
as.POSIXlt(Sys.time(), "EST" )   # somewhere in Eastern Canada
as.POSIXlt(Sys.time(), "HST")    # in Hawaii
as.POSIXlt(Sys.time(), "Australia/Darwin")
}
#ifdef windows

cols <- c("code", "coordinates", "TZ", "comments")
tmp <- read.delim(file.path(R.home(), "share", "zoneinfo", "zone.tab"),
                  header = FALSE, comment.char="#", col.names = cols)
if(interactive()) View(tmp)
#endif
}
\keyword{utilities}
\keyword{chron}
