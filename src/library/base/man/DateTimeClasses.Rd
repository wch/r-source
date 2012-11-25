% File src/library/base/man/DateTimeClasses.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
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
  representing calendar dates and times (to the nearest second).
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
  (in the UTC timezone) as a numeric vector.  Class \code{"POSIXlt"} is
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
    \item{\code{isdst}}{Daylight Savings Time flag.  Positive if in
      force, zero if not, negative if unknown.}
  }
  Note that the internal list structure is somewhat hidden, as many
  methods, including \code{\link{print}()} or \code{\link{str}}, apply
  to the abstract date-time vector, as for \code{"POSIXct"}.
  The classes correspond to the POSIX/C99 constructs of \sQuote{calendar
    time} (the \code{time_t} data type) and \sQuote{local time} (or
  broken-down time, the \code{struct tm} data type), from which they
  also inherit their names.  The components of \code{"POSIXlt"} are
  integer vectors, except \code{sec}.

  \code{"POSIXct"} is more convenient for including in data frames, and
  \code{"POSIXlt"} is closer to human-readable forms.  A virtual class
  \code{"POSIXt"} exists from which both of the classes inherit: it is
  used to allow operations such as subtraction to mix the two classes.
  Note that \code{\link{length}(x)} is the length of the corresponding
  (abstract) date/time vector, also in the \code{"POSIXlt"} case.

  Components \code{wday} and \code{yday} of \code{"POSIXlt"} are for
  information, and are not used in the conversion to calendar time.
  However, \code{isdst} is needed to distinguish times at the end of
  DST: typically 1am to 2am occurs twice, first in DST and then in
  standard time.  At all other times \code{isdst} can be deduced from
  the first six values, but the behaviour if it is set incorrectly is
  platform-dependent.
  % POSIX says used 'initially', so it should probably be ignored.
  % glibc has a complicated fixup.

  Logical comparisons and limited arithmetic are available for both
  classes.  One can add or subtract a number of seconds from a date-time
  object, but not add two date-time objects.  Subtraction of two
  date-time objects is equivalent to using \code{\link{difftime}}.  Be
  aware that \code{"POSIXlt"} objects will be interpreted as being in
  the current timezone for these operations, unless a timezone has been
  specified.

  \code{"POSIXlt"} objects will often have an attribute \code{"tzone"},
  a character vector of length 3 giving the timezone name from the
  \env{TZ} environment variable and the names of the base timezone
  and the alternate (daylight-saving) timezone.  Sometimes this may
  just be of length one, giving the timezone name.

  \code{"POSIXct"} objects may also have an attribute \code{"tzone"}, a
  character vector of length one.  If set to a non-empty value, it will
  determine how the object is converted to class \code{"POSIXlt"} and in
  particular how it is printed.  This is usually desirable, but if you
  want to specify an object in a particular timezone but to be printed
  in the current timezone you may want to remove the \code{"tzone"}
  attribute (e.g. by \code{c(x)}).

  Unfortunately, the conversion is complicated by the operation of time
  zones and leap seconds (24 days have been 86401 seconds long so far:
  the times of the extra seconds are in the object \code{.leap.seconds}).
  The details of this are entrusted to the OS services where possible.
  This always covers the period 1970--2037, and on most machines
  back to 1902 (when time zones were in their infancy).  Outside
  the platform limits we use our own C code.  This uses the offset from
  GMT in use either for 1902 (when there was no DST) or that predicted
  for one of 2030 to 2037 (chosen so that the likely DST transition days
  are Sundays), and uses the alternate (daylight-saving) timezone only
  if \code{isdst} is positive or (if \code{-1}) if DST was predicted to
  be in operation in the 2030s on that day.  (There is no reason to
  suppose that the DST rules will remain the same in the future, and
  indeed the US legislated in 2005 to change its rules as from 2007,
  with a possible future reversion.)

  It seems that some rare systems use leap seconds, but most ignore
  them (as required by POSIX).  This is detected and corrected for at
  build time, so all \code{"POSIXct"} times used by \R do not include
  leap seconds. (Conceivably this could be wrong if the system has
  changed since build time, just possibly by changing locales or the
  \file{zoneinfo} database.)

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
\section{Warning}{
  Some Unix-like systems (especially Linux ones) do not have environment
  variable \env{TZ}
  set, yet have internal code that expects it (as does POSIX).  We have
  tried to work around this, but if you get unexpected results try
  setting \env{TZ}.  See \code{\link{Sys.timezone}} for valid settings.
}
\examples{\donttest{% <-->  may fail on platforms wrongly set up
(z <- Sys.time())             # the current date, as class "POSIXct"

Sys.time() - 3600             # an hour ago

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
format(.leap.seconds)         # all 24 leap seconds in your timezone
print(.leap.seconds, tz = "PST8PDT")  # and in Seattle's

## look at *internal* representation of "POSIXlt" :
leapS <- as.POSIXlt(.leap.seconds)
names(leapS) ; is.list(leapS)
## str() "too smart" -->  need unclass(.):
utils::str(unclass(leapS), vec.len = 7)
}}
\keyword{utilities}
\keyword{chron}
