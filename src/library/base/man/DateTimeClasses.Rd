% File src/library/base/man/DateTimeClasses.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{DateTimeClasses}
% implementation mostly in ../R/datetime.R
\alias{DateTimeClasses}
\alias{POSIXct}
\alias{POSIXlt}
\alias{POSIXt}
\alias{print.POSIXct}
\alias{print.POSIXlt}
\alias{summary.POSIXct}
\alias{summary.POSIXlt}
\alias{+.POSIXt}
\alias{-.POSIXt}
\alias{Ops.POSIXt}
\alias{Math.POSIXt}
\alias{Summary.POSIXct}
\alias{Math.POSIXlt}
\alias{Summary.POSIXlt}
\alias{[.POSIXct}
\alias{[<-.POSIXct}
\alias{[[.POSIXct}
\alias{[.POSIXlt}
\alias{[<-.POSIXlt}
\alias{as.data.frame.POSIXct}
\alias{as.data.frame.POSIXlt}
\alias{as.list.POSIXct}
\alias{.leap.seconds}
\alias{anyNA.POSIXlt}
\alias{is.na.POSIXlt}
\alias{c.POSIXct}
\alias{c.POSIXlt}
\alias{as.matrix.POSIXlt}
\alias{length.POSIXlt}
\alias{mean.POSIXct}
\alias{mean.POSIXlt}
\alias{str.POSIXt}
\alias{check_tzones}
\alias{duplicated.POSIXlt}
\alias{unique.POSIXlt}
\alias{split.POSIXct}
\alias{names.POSIXlt}
\alias{names<-.POSIXlt}

\alias{date-time} % linked to in difftime.Rd

\title{Date-Time Classes}
\description{
  Description of the classes \code{"POSIXlt"} and \code{"POSIXct"}
  representing calendar dates and times.
}
\usage{
\method{print}{POSIXct}(x, \dots)

\method{summary}{POSIXct}(object, digits = 15, \dots)

\special{time + z}
\special{z + time}
\special{time - z}
\special{time1 lop time2}
}
\arguments{
  \item{x, object}{An object to be printed or summarized from one of the
    date-time classes.}
  \item{digits}{Number of significant digits for the computations:
    should be high enough to represent the least important time unit
    exactly.}
  \item{\dots}{Further arguments to be passed from or to other methods.}
  \item{time}{date-time objects}
  \item{time1, time2}{date-time objects or character vectors.  (Character
    vectors are converted by \code{\link{as.POSIXct}}.)}
  \item{z}{a numeric vector (in seconds)}
  \item{lop}{One of \code{==}, \code{!=}, \code{<}, \code{<=}, \code{>}
    or \code{>=}.}
}
\details{
  There are two basic classes of date/times.  Class \code{"POSIXct"}
  represents the (signed) number of seconds since the beginning of 1970
  (in the UTC time zone) as a numeric vector.  Class \code{"POSIXlt"} is
  a named list of vectors representing
  \describe{
    \item{\code{sec}}{0--61: seconds}
    \item{\code{min}}{0--59: minutes}
    \item{\code{hour}}{0--23: hours}
    \item{\code{mday}}{1--31: day of the month}
    \item{\code{mon}}{0--11: months after the first of the year.}
    \item{\code{year}}{years since 1900.}
    \item{\code{wday}}{0--6 day of the week, starting on Sunday.}
    \item{\code{yday}}{0--365: day of the year.}
    \item{\code{isdst}}{Daylight Saving Time flag.  Positive if in
      force, zero if not, negative if unknown.}
    \item{\code{zone}}{(Optional.) The abbreviation for the time zone in
      force at that time: \code{""} is unknown.}
    \item{\code{gmtoff}}{(Optional.) The offset in minutes from GMT:
      positive values are East of the meridian.  Usually \code{NA} if
      unknown, but \code{0} could mean unknown.}
  }
  (The last two components not present for times in UTC and are
  platform-dependent: they are supported on platforms based on BSD or
  \code{glibc}, including Linux and OS X, but are not necessarily set.).
  Note that the internal list structure is somewhat hidden, as many
  methods (including \code{\link{length}(x)}, \code{\link{print}()} and
  \code{\link{str}}) apply to the abstract date-time vector, as for
  \code{"POSIXct"}.  The classes correspond to the POSIX/C99 constructs
  of \sQuote{calendar time} (the \code{time_t} data type) and
  \sQuote{local time} (or broken-down time, the \code{struct tm} data
  type), from which they also inherit their names.  The components of
  \code{"POSIXlt"} are integer vectors, except \code{sec} and
  \code{zone}.

  \code{"POSIXct"} is more convenient for including in data frames, and
  \code{"POSIXlt"} is closer to human-readable forms.  A virtual class
  \code{"POSIXt"} exists from which both of the classes inherit: it is
  used to allow operations such as subtraction to mix the two classes.

  Components \code{wday} and \code{yday} of \code{"POSIXlt"} are for
  information, and are not used in the conversion to calendar time.
  However, \code{isdst} is needed to distinguish times at the end of
  DST: typically 1am to 2am occurs twice, first in DST and then in
  standard time.  At all other times \code{isdst} can be deduced from
  the first six values, but the behaviour if it is set incorrectly is
  platform-dependent.
  % POSIX says used 'initially', so it should probably be ignored.
  % glibc has a complicated fixup.

  Logical comparisons and some arithmetic operations are available for
  both classes.  One can add or subtract a number of seconds from a
  date-time object, but not add two date-time objects.  Subtraction of
  two date-time objects is equivalent to using \code{\link{difftime}}.
  Be aware that \code{"POSIXlt"} objects will be interpreted as being in
  the current time zone for these operations unless a time zone has been
  specified.

  \code{"POSIXlt"} objects will often have an attribute \code{"tzone"},
  a character vector of length 3 giving the time zone name from the
  \env{TZ} environment variable and the names of the base time zone
  and the alternate (daylight-saving) time zone.  Sometimes this may
  just be of length one, giving the \link{time zone} name.

  \code{"POSIXct"} objects may also have an attribute \code{"tzone"}, a
  character vector of length one.  If set to a non-empty value, it will
  determine how the object is converted to class \code{"POSIXlt"} and in
  particular how it is printed.  This is usually desirable, but if you
  want to specify an object in a particular time zone but to be printed
  in the current time zone you may want to remove the \code{"tzone"}
  attribute (e.g. by \code{c(x)}).

  Unfortunately, the conversion is complicated by the operation of time
  zones and leap seconds (25 days have been 86401 seconds long so far:
  the times of the extra seconds are in the object
  \code{.leap.seconds}).  The details of this are entrusted to the OS
  services where possible.  It seems that some rare systems used to use
  leap seconds, but all known current platforms ignore them (as required
  by POSIX).  This is detected and corrected for at build time, so
  \code{"POSIXct"} times used by \R do not include leap seconds on any
  platform.

  Using \code{\link{c}} on \code{"POSIXlt"} objects converts them to the
  current time zone, and on \code{"POSIXct"} objects drops any
  \code{"tzone"} attributes (even if they are all marked with the same
  time zone).

  A few times have specific issues.  First, the leap seconds are ignored,
  and real times such as \code{"2005-12-31 23:59:60"} are (probably)
  treated as the next second.  However, they will never be generated by
  \R, and are unlikely to arise as input.  Second, on some OSes there is
  a problem in the POSIX/C99 standard with \code{"1969-12-31 23:59:59 UTC"},
  which is \code{-1} in calendar time and that value is on those OSes
  also used as an error code.  Thus \code{as.POSIXct("1969-12-31
  23:59:59", format = "\%Y-\%m-\%d \%H:\%M:\%S", tz = "UTC")} may give
  \code{NA}, and hence \code{as.POSIXct("1969-12-31 23:59:59",
  tz = "UTC")} will give \code{"1969-12-31 23:59:00"}.  Other OSes
  (including the code used by \R on Windows) report errors separately
  and so are able to handle that time as valid.

  The print methods respect \code{\link{options}("max.print")}.
}
\section{Sub-second Accuracy}{
  Classes \code{"POSIXct"} and  \code{"POSIXlt"} are able to express
  fractions of a second.  (Conversion of fractions between the two forms
  may not be exact, but will have better than microsecond accuracy.)

  Fractional seconds are printed only if
  \code{\link{options}("digits.secs")} is set: see \code{\link{strftime}}.
}
\section{Valid ranges for times}{
  The \code{"POSIXlt"} class can represent a very wide range of times (up
  to billions of years), but such times can only be interpreted with
  reference to a time zone.

  % http://www.timeanddate.com/calendar/julian-gregorian-switch.html The
  concept of time zones was first adopted in the nineteenth century, and
  the Gregorian calendar was introduced in 1582 but not universally
  adopted until 1927.  OS services almost invariably assume the
  Gregorian calendar and may assume that the time zone that was first
  enacted for the location was in force before that date.  (The earliest
  legislated time zone seems to have been London on 1847-12-01.)  Some
  OSes assume the previous use of \sQuote{local time} based on the
  longitude of a location within the time zone.
  % from the Olson files.
  
  Most operating systems represent \code{POSIXct} times as C type
  \code{long}. This means that on 32-bit OSes this covers the period
  1902 to 2037.  On all known 64-bit platforms and for the code we use
  on 32-bit Windows, the range of representable times is billions of
  years: however, not all can convert correctly times before 1902 or
  after 2037.  A few benighted OSes used a unsigned type and so cannot
  represent times before 1970.
  % glibc in 2003/4, AIX?

  Where possible the platform limits are detected, and outside
  the limits we use our own C code.  This uses the offset from
  GMT in use either for 1902 (when there was no DST) or that predicted
  for one of 2030 to 2037 (chosen so that the likely DST transition days
  are Sundays), and uses the alternate (daylight-saving) time zone only
  if \code{isdst} is positive or (if \code{-1}) if DST was predicted to
  be in operation in the 2030s on that day.

  Note that there are places (e.g. Rome) whose offset from UTC varied
  in the years prior to 1902, and these will be handled correctly only
  where there is OS support.

  There is no reason to suppose that the DST rules will remain the same
  in the future, and indeed the US legislated in 2005 to change its
  rules as from 2007, with a possible future reversion.  So conversions
  for times more than a year or two ahead are speculative.  
}
\seealso{
  \link{Dates} for dates without times.

  \code{\link{as.POSIXct}} and \code{\link{as.POSIXlt}} for conversion
  between the classes.

  \code{\link{strptime}} for conversion to and from character
  representations.

  \code{\link{Sys.time}} for clock time as a \code{"POSIXct"} object.

  \code{\link{difftime}} for time intervals.

  \code{\link{cut.POSIXt}}, \code{\link{seq.POSIXt}},
  \code{\link{round.POSIXt}} and \code{\link{trunc.POSIXt}} for methods
  for these classes.

  \code{\link{weekdays}} for convenience extraction functions.
}
\references{
  Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News},
  \bold{1/2}, 8--11.
  \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}
}

\section{Warnings}{
  Some Unix-like systems (especially Linux ones) do not have environment
  variable \env{TZ} set, yet have internal code that expects it (as does
  POSIX).  We have tried to work around this, but if you get unexpected
  results try setting \env{TZ}.  See \code{\link{Sys.timezone}} for
  valid settings.

  Great care is needed when comparing objects of class \code{"POSIXlt"}.
  Not only are components and attributes optional; several components
  may have values meaning \sQuote{not yet determined} and the same time
  represented in different time zones will look quite different.
}
\examples{\donttest{% <-->  may fail on platforms wrongly set up
(z <- Sys.time())             # the current date, as class "POSIXct"

Sys.time() - 3600             # an hour ago

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
format(.leap.seconds)         # the leap seconds in your time zone
print(.leap.seconds, tz = "PST8PDT")  # and in Seattle's

## look at *internal* representation of "POSIXlt" :
leapS <- as.POSIXlt(.leap.seconds)
names(leapS) ; is.list(leapS)
## str() "too smart" -->  need unclass(.):
utils::str(unclass(leapS), vec.len = 7)
}}
\keyword{utilities}
\keyword{chron}
