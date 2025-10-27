% File src/library/base/man/DateTimeClasses.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
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
\alias{$<-.POSIXlt}
\alias{[.POSIXct}
\alias{[<-.POSIXct}
\alias{[[.POSIXct}
\alias{[.POSIXlt}
\alias{[<-.POSIXlt}
\alias{[[.POSIXlt}
\alias{[[<-.POSIXlt}
\alias{as.data.frame.POSIXlt}
\alias{as.list.POSIXct}
\alias{as.list.POSIXlt}
\alias{.leap.seconds}
\alias{anyNA.POSIXlt}
\alias{is.finite.POSIXlt}
\alias{is.infinite.POSIXlt}
\alias{is.na.POSIXlt}
\alias{is.nan.POSIXlt}
\alias{c.POSIXct}
\alias{c.POSIXlt}
\alias{as.matrix.POSIXlt}
\alias{length.POSIXlt}
\alias{length<-.POSIXct}
\alias{length<-.POSIXlt}
\alias{mean.POSIXct}
\alias{mean.POSIXlt}
\alias{str.POSIXt}
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
\method{print}{POSIXct}(x, tz = "", usetz = TRUE, max = NULL,
      digits = getOption("digits.secs"), \dots)

\method{summary}{POSIXct}(object, digits = 15, \dots)

\special{time + z}
\special{z + time}
\special{time - z}
\special{time1 lop time2}

.leap.seconds
}
\arguments{
  \item{x, object}{an object to be printed or summarized from one of the
    date-time classes.}
  \item{tz, usetz}{for timezone formatting, passed to \code{\link{format.POSIXct}}.}
  \item{max}{numeric or \code{NULL}, specifying the maximal number of
    entries to be printed.  By default, when \code{NULL},
    \code{\link{getOption}("max.print")} used.}
  \item{digits}{number of digits to format fractional seconds in the case
    of \code{print()};
    number of \emph{significant} digits for the computations:
    should be high enough to represent the least important time unit
    exactly.}
  \item{\dots}{further arguments to be passed from or to other methods.}
  \item{time}{date-time objects.}
  \item{time1, time2}{date-time objects or character vectors.  (Character
    vectors are converted by \code{\link{as.POSIXct}}.)}
  \item{z}{a numeric vector (in seconds).}
  \item{lop}{one of \code{==}, \code{!=}, \code{<}, \code{<=}, \code{>}
    or \code{>=}.}
}
\details{
  There are two basic classes of date/times.  Class \code{"POSIXct"}
  represents the (signed) number of seconds since the beginning of 1970
  (in the UTC time zone) as a numeric vector.  Class \code{"POSIXlt"} is
  internally a \code{\link{list}} of vectors with components named
  \code{sec}, \code{min}, \code{hour} for the time,
  \code{mday}, \code{mon}, and \code{year}, for the date,
  \code{wday}, \code{yday} for the day of the week and day of the year,
  \code{isdst}, a Daylight Saving Time flag,
  and sometimes (both \emph{optional})
  \code{zone}, a string for the time zone, and
  \code{gmtoff}, offset in seconds from GMT,
  see the section \sQuote{Details on \I{POSIXlt}} below for more details.

  The classes correspond to the POSIX/C99 constructs of \sQuote{calendar
    time} (the \code{time_t} data type, \dQuote{\I{ct}}), and \sQuote{local time}
  (or broken-down time, the \samp{struct tm} data type, \dQuote{\I{lt}}),
  from which they also inherit their names.

  \code{"POSIXct"} is more convenient for including in data frames, and
  \code{"POSIXlt"} is closer to human-readable forms.  A virtual class
  \code{"POSIXt"} exists from which both of the classes inherit: it is
  used to allow operations such as subtraction to mix the two classes.

  Logical comparisons and some arithmetic operations are available for
  both classes.  One can add or subtract a number of seconds from a
  date-time object, but not add two date-time objects.  Subtraction of
  two date-time objects is equivalent to using \code{\link{difftime}}.
  Be aware that \code{"POSIXlt"} objects will be interpreted as being in
  the current time zone for these operations unless a time zone has been
  specified.

  Further implemented methods comprise \code{\link{mean}},
  \code{\link{summary}}, \code{\link{unique}}, \code{\link{is.finite}},
  \code{\link{length<-}}, \code{\link{pretty}},\dots; also
  \pkg{graphics} package's \code{\link{hist}} and \code{\link{Axis}};
  use \code{\link{methods}(class = "POSIXt")}, \code{.. class = "POSIXlt"}
  and \code{"POSIXct"} to see more.

  Both classes may have an attribute \code{"tzone"}, specifying the time
  zone.  Note however that their meaning differ, see the section
  \sQuote{Time Zones} below for more details.

  % NB: Do *NOT* change the line structure before the two \Sexpr{}s below, unless
  % --  you update the Rd2HTML() regression check in ../../../../tests/reg-tests-1d.R :
  Unfortunately, the conversion is complicated by the operation of time
  zones and leap seconds (according to this version of \R's data,
  \Sexpr{length(.leap.seconds)} days have been 86401 seconds long so
  far, the last being on (actually, immediately before)
  \Sexpr{format(as.Date(utils::tail(.leap.seconds, 1)))}: the times of the
  extra seconds are in the object \code{.leap.seconds}).  The details of
  this are entrusted to the OS services where possible.  It seems that
  some rare systems used to use leap seconds, but all known current
  platforms ignore them (as required by POSIX).  This is detected and
  corrected for at build time, so \code{"POSIXct"} times used by \R do
  not include leap seconds on any platform.

  Using \code{\link{c}} on \code{"POSIXlt"} objects converts them to the
  current time zone, and on \code{"POSIXct"} objects drops \code{"tzone"}
  attributes if they are not all the same.

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
}% end{details}

\section{Time zones}{
  \code{"POSIXlt"} objects will often have an attribute \code{"tzone"},
  a character vector of length 3 giving the time zone name (from the \env{TZ}
  environment variable or argument \code{tz} of functions creating
  \code{"POSIXlt"} objects; \code{""} marks the current time zone)
  and the names of the base time zone
  and the alternate (daylight-saving) time zone.  Sometimes this may
  just be of length one, giving the \link{time zone} name.

  \code{"POSIXct"} objects may also have an attribute \code{"tzone"}, a
  character vector of length one.  If set to a non-empty value, it will
  determine how the object is converted to class \code{"POSIXlt"} and in
  particular how it is printed.  This is usually desirable, but if you
  want to specify an object in a particular time zone but to be printed
  in the current time zone you may want to remove the \code{"tzone"}
  attribute. % No longer true: (e.g., by \code{c(x)}).
}

\section{Details on \I{POSIXlt}}{
  Class \code{"POSIXlt"} is internally a named \code{\link{list}} of
  vectors representing date-times, with the following list components
  \describe{
    \item{\code{sec}}{0--61: seconds, allowing for leap seconds.}
    \item{\code{min}}{0--59: minutes.}
    \item{\code{hour}}{0--23: hours.}
    \item{\code{mday}}{1--31: day of the month.}
    \item{\code{mon}}{0--11: months after the first of the year.}
    \item{\code{year}}{years since 1900.}
    \item{\code{wday}}{0--6 day of the week, starting on Sunday.}
    \item{\code{yday}}{0--365: day of the year (365 only in leap years).}
    \item{\code{isdst}}{Daylight Saving Time flag.  Positive if in
      force, zero if not, negative if unknown.}
    \item{\code{zone}}{(Optional.) The abbreviation for the time zone in
      force at that time: \code{""} if unknown (but \code{""} might also
      be used for UTC).}
    \item{\code{gmtoff}}{(Optional.) The offset in seconds from GMT:
      positive values are East of the meridian.  Usually \code{NA} if
      unknown, but \code{0} could mean unknown.}
  }
  The components must be in this order: that was only minimally checked
  prior to \R 4.3.0.  All objects created in \R 4.3.0 have the optional
  components.  From earlier versions of \R, the last two components will
  not be present for times in UTC and are platform-dependent.  Currently
  \code{gmtoff} is set on almost all current platforms: those based on
  BSD or \code{glibc} (including Linux and macOS) and those using the
  \code{tzcode} implementation shipped with \R (including Windows and by
  default macOS).

  Note that the internal list structure is somewhat hidden, as many
  methods (including \code{\link{length}()}, \code{\link{print}()} and
  \code{\link{str}()}) apply to the abstract date-time vector, as for
  \code{"POSIXct"}.  One can extract and replace \emph{single}
  components via \code{[} indexing with \emph{two} indices (see the
  examples).

  The components of \code{"POSIXlt"} are \code{\link{integer}} vectors,
  except \code{sec} (\code{\link{double}}) and \code{zone}
  (\code{\link{character}}).  However most users will coerce numeric
  values for the first to real and the rest bar \code{zone} to integer.

  Components \code{wday} and \code{yday} are for information, and are not
  used in the conversion to calendar time nor for printing,
  \code{format()}, or in \code{as.character()}.

  However, component \code{isdst} is needed to distinguish times at the
  end of DST: typically 1am to 2am occurs twice, first in DST and then
  in standard time.  At all other times \code{isdst} can be deduced from
  the first six values, but the behaviour if it is set incorrectly is
  platform-dependent.  For example Linux/\I{glibc} when checked fixed up
  incorrect values in time zones which support DST but gave an error on
  value \code{1} in those without DST.
  % POSIX says used 'initially', so it should probably be ignored.
  % glibc has a complicated fixup.

  For \dQuote{ragged} and out-of-range vs \dQuote{balanced}
  \code{"POSIXlt"} objects, see \code{\link{balancePOSIXlt}()}.
}% POSIXlt

\section{Sub-second Accuracy}{
  Classes \code{"POSIXct"} and  \code{"POSIXlt"} are able to express
  fractions of a second where the latter allows for higher accuracy.
  Consequently, conversion of fractions between the two forms
  may not be exact, but will have better than microsecond accuracy.

  Fractional seconds are printed only if
  \code{\link{options}("digits.secs")} is set: see \code{\link{strftime}}.
}
\section{Valid ranges for times}{
  The \code{"POSIXlt"} class can represent a very wide range of times (up
  to billions of years), but such times can only be interpreted with
  reference to a time zone.

  % https://www.timeanddate.com/calendar/julian-gregorian-switch.html
  The concept of time zones was first adopted in the nineteenth century,
  and the Gregorian calendar was introduced in 1582 but not universally
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

  Note that there are places (e.g., Rome) whose offset from UTC varied
  in the years prior to 1902, and these will be handled correctly only
  where there is OS support.

  There is no reason to assume that the DST rules will remain the same
  in the future: the US legislated in 2005 to change its
  rules as from 2007, with a possible future reversion.  So conversions
  for times more than a year or two ahead are speculative.  Other
  countries have changed their rules (and indeed, if DST is used at all)
  at a few days' notice.  So representations and conversion of future
  dates are tentative.  This also applies to dates after the in-use
  version of the time-zone database -- not all platforms keep it up to
  date, which includes that shipped with older versions of \R where used
  (which it is by default on Windows and macOS).
}% ranges of times
\seealso{
  \link{Dates} for dates without times.

  \code{\link{as.POSIXct}} and \code{\link{as.POSIXlt}} for conversion
  between the classes.

  \code{\link{strptime}} for conversion to and from character
  representations.

  \code{\link{Sys.time}} for clock time as a \code{"POSIXct"} object.

  \code{\link{difftime}} for time intervals.

  \code{\link{balancePOSIXlt}()} for balancing or filling \dQuote{ragged}
  \I{POSIXlt} objects.

  \code{\link{cut.POSIXt}}, \code{\link{seq.POSIXt}},
  \code{\link{round.POSIXt}} and \code{\link{trunc.POSIXt}} for methods
  for these classes.

  \code{\link{weekdays}}, for convenience extraction functions (also
  including \code{months()}, \code{quarters()}, \code{julian()}).
}
\references{
  Ripley, B. D. and Hornik, K. (2001).
  \dQuote{Date-time classes.}
  \emph{R News}, \bold{1}(2), 8--11.
  \url{https://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}.
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

  The \emph{order} of the list components of \code{"POSIXlt"} objects
  must not be changed, as several C-based conversion methods rely on the
  order for efficiency.

  Do not use \code{\link{identical}()} on objects of class \code{"POSIXct"},
  rather \code{\link{==}} or \code{\link{all.equal}}.
  Their \code{\link{storage.mode}} may be \code{"double"} or
  \code{"integer"} and you should not rely on which is chosen.
}
\examples{\dontdiff{
(z <- Sys.time())             # the current date, as class "POSIXct"

Sys.time() - 3600             # an hour ago

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
format(.leap.seconds)         # the leap seconds in your time zone}
print(.leap.seconds, tz = "America/Los_Angeles")  # and in Seattle's

## look at *internal* representation of "POSIXlt" :
leapS <- as.POSIXlt(.leap.seconds)
names(unclass(leapS)) ; is.list(leapS)
## str() on inner structure needs unclass(.):
utils::str(unclass(leapS), vec.len = 7)
## show all (apart from "tzone" attr):
data.frame(unclass(leapS))

## Extracting *single* components of POSIXlt objects:
leapS[1 : 5, "year"]
leapS[17:22, "mon" ]
}
\keyword{utilities}
\keyword{chron}
