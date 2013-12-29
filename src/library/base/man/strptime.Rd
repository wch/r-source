% File src/library/base/man/strptime.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{strptime}
\alias{format.POSIXct}
\alias{format.POSIXlt}
\alias{strftime}
\alias{strptime}
\alias{as.character.POSIXt}
\title{Date-time Conversion Functions to and from Character}
\description{
  Functions to convert between character representations and objects of
  classes \code{"POSIXlt"} and \code{"POSIXct"} representing calendar
  dates and times.
}
\usage{
\method{format}{POSIXct}(x, format = "", tz = "", usetz = FALSE, \dots)
\method{format}{POSIXlt}(x, format = "", usetz = FALSE, \dots)

\method{as.character}{POSIXt}(x, \dots)

strftime(x, format = "", tz = "", usetz = FALSE, \dots)
strptime(x, format, tz = "")
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A character string specifying the time zone to be used for
    the conversion.  System-specific (see \code{\link{as.POSIXlt}}), but
    \code{""} is the current time zone, and \code{"GMT"} is UTC.}
  \item{format}{A character string.  The default for the \code{format}
    methods is
    \code{"\%Y-\%m-\%d \%H:\%M:\%S"} if any component has a time
    component which is not midnight, and \code{"\%Y-\%m-\%d"}
    otherwise.  If \code{\link{options}("digits.secs")} is set, up to
    the specified number of digits will be printed for seconds.}
  \item{\dots}{Further arguments to be passed from or to other methods.}
  \item{usetz}{logical.  Should the time zone be appended to the output?
    This is used in printing times, and as a workaround for problems with
    using \code{"\%Z"} on some Linux systems.}
}
\details{
  The \code{format} and \code{as.character} methods and \code{strftime}
  convert objects from the classes \code{"POSIXlt"} and \code{"POSIXct"}
  (not \code{strftime}) to character vectors.

  \code{strptime} converts character vectors to class \code{"POSIXlt"}:
  its input \code{x} is first converted by \code{\link{as.character}}.
  Each input string is processed as far as necessary for the format
  specified: any trailing characters are ignored.

  \code{strftime} is a wrapper for \code{format.POSIXlt}, and it and
  \code{format.POSIXct} first convert to class \code{"POSIXlt"} by
  calling \code{\link{as.POSIXlt}}.  Note that only that conversion
  depends on the time zone.

  The usual vector re-cycling rules are applied to \code{x} and
  \code{format} so the answer will be of length that of the longer of the
  vectors.

  Locale-specific conversions to and from character strings are used
  where appropriate and available.  This affects the names of the days
  and months, the AM/PM indicator (if used) and the separators in
  formats such as \code{\%x} and \code{\%X} (via the setting of the
  \code{LC_TIME} locale category).

  The details of the formats are system-specific, but the following are
  defined by the ISO C99 / POSIX standard for \code{strftime} and are
  likely to be widely available.  A \emph{conversion specification} is
  introduced by \code{\%}, usually followed by a single letter or
  \code{O} or \code{E} and then a single letter.
  Any character in the format string not part of a conversion specification
  is interpreted literally (and \code{\%\%} gives \code{\%}).  Widely
  implemented conversion specifications include
  \describe{
    \item{\code{\%a}}{Abbreviated weekday name in the current
      locale.  (Also matches full name on input.)}
    \item{\code{\%A}}{Full weekday name in the current locale.  (Also
      matches abbreviated name on input.)}
    \item{\code{\%b}}{Abbreviated month name in the current
      locale.  (Also matches full name on input.)}
    \item{\code{\%B}}{Full month name in the current locale.  (Also
      matches abbreviated name on input.)}
    \item{\code{\%c}}{Date and time.   Locale-specific on output,
      \code{"\%a \%b \%e \%H:\%M:\%S \%Y"} on input.}
    \item{\code{\%d}}{Day of the month as decimal number (01--31).}
    \item{\code{\%H}}{Hours as decimal number (00--23).  As a special
      exception times such as \samp{24:00:00} are accepted for input,
      since ISO 8601 allows these.}
    \item{\code{\%I}}{Hours as decimal number (01--12).}
    \item{\code{\%j}}{Day of year as decimal number (001--366).}
    \item{\code{\%m}}{Month as decimal number (01--12).}
    \item{\code{\%M}}{Minute as decimal number (00--59).}
    \item{\code{\%p}}{AM/PM indicator in the locale.  Used in
      conjunction with \code{\%I} and \bold{not} with \code{\%H}.  An
      empty string in some locales.}
    \item{\code{\%S}}{Second as decimal number (00--61), allowing for
      up to two leap-seconds (but POSIX-compliant implementations
      will ignore leap seconds).}
    % see http://en.wikipedia.org/wiki/Week_number#Week_number
    \item{\code{\%U}}{Week of the year as decimal number (00--53) using
      Sunday as the first day 1 of the week (and typically with the
      first Sunday of the year as day 1 of week 1).  The US convention.}
    \item{\code{\%w}}{Weekday as decimal number (0--6, Sunday is 0).}
    \item{\code{\%W}}{Week of the year as decimal number (00--53) using
      Monday as the first day of week (and typically with the
      first Monday of the year as day 1 of week 1).  The UK convention.}
    \item{\code{\%x}}{Date.  Locale-specific on output,
      \code{"\%y/\%m/\%d"} on input.}
    \item{\code{\%X}}{Time.  Locale-specific on output,
      \code{"\%H:\%M:\%S"} on input.}
    \item{\code{\%y}}{Year without century (00--99).  On input, values
      00 to 68 are prefixed by 20 and 69 to 99 by 19 -- that is the
      behaviour specified by the 2004 and 2008 POSIX standards, but they do
      also say \sQuote{it is expected that in a future version the
	default century inferred from a 2-digit year will change}.}
    \item{\code{\%Y}}{Year with century.  Note that whereas there was no
      zero in the original Gregorian calendar, ISO 8601:2004 defines it
      to be valid (interpreted as 1BC): see
      \url{http://en.wikipedia.org/wiki/0_(year)}.  Note that the standard
      also says that years before 1582 in its calendar should only be used
      with agreement of the parties involved.}
    \item{\code{\%z}}{Signed offset in hours and minutes from UTC, so
      \code{-0800} is 8 hours behind UTC. Note that offsets are not
      necessarily in whole minutes, and that this may output
      \code{+0000} when offsets are unknown.}
    \item{\code{\%Z}}{(output only.) Time zone abbreviation as a
      character string (empty if not available).  This may not be reliable
      when a time zone has changed abbreviations over the years.}
  }
  Where leading zeros are shown they will be used on output but are
  optional on input.

  Note that when \code{\%z} or \code{\%Z} is used for output with an
  object with an assigned time zone an attempt is made to use the values
  for that time zone --- but it is not guaranteed to succeed.

  Also defined in the current standards but less widely implemented
  (e.g. not for output on Windows) are
  \describe{
    \item{\code{\%C}}{Century (00--99): the integer part of the year
      divided by 100.}
    \item{\code{\%D}}{Date format such as \code{\%m/\%d/\%y}: ISO C99
      says it should be that exact format.}
    \item{\code{\%e}}{Day of the month as decimal number (1--31), with
      a leading space for a single-digit number.}
    \item{\code{\%F}}{Equivalent to \%Y-\%m-\%d (the ISO 8601 date
      format).}
    \item{\code{\%g}}{The last two digits of the week-based year
      (see \code{\%V}).  (Accepted but ignored on input.)}
    \item{\code{\%G}}{The week-based year (see \code{\%V}) as a decimal
      number.  (Accepted but ignored on input.)}
    \item{\code{\%h}}{Equivalent to \code{\%b}.}
    \item{\code{\%k}}{The 24-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%l}}{The 12-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%n}}{Newline on output, arbitrary whitespace on input.}
    \item{\code{\%r}}{The 12-hour clock time (using the locale's AM or
      PM).  Only defined in some locales.}
    \item{\code{\%R}}{Equivalent to \code{\%H:\%M}.}
    \item{\code{\%s}}{(Output only.) The number of seconds since the
      epoch. Not POSIX, not widely implemented.}
    \item{\code{\%t}}{Tab on output, arbitrary whitespace on input.}
    \item{\code{\%T}}{Equivalent to \code{\%H:\%M:\%S}.}
    \item{\code{\%u}}{Weekday as a decimal number (1--7, Monday is 1).}
    \item{\code{\%V}}{Week of the year as decimal number (00--53) as
      defined in ISO 8601.
      If the week (starting on Monday) containing 1 January has four or
      more days in the new year, then it is considered week 1.  Otherwise, it
      is the last week of the previous year, and the next week is week
      1.  (Accepted but ignored on input.)}
  }
  For output there are also \code{\%O[dHImMUVwWy]} which may emit
  numbers in an alternative locale-dependent format (e.g. roman
  numerals), and \code{\%E[cCyYxX]} which can use an alternative
  \sQuote{era} (e.g. a different religious calendar).  Which of these
  are supported is OS-dependent.  These are accepted for input, but with
  the standard interpretation.

  Specific to \R is \code{\%OSn}, which for output gives the seconds
  truncated to \code{0 <= n <= 6} decimal places (and if \code{\%OS} is
  not followed by a digit, it uses the setting of
  \code{\link{getOption}("digits.secs")}, or if that is unset, \code{n =
  3}).  Further, for \code{strptime} \code{\%OS} will input seconds
  including fractional seconds.  Note that \code{\%S} ignores (and not
  rounds) fractional parts on output.

  Even when conversion specifications are implemented they may not be
  implemented correctly: \code{\%z} seems particularly unreliable.

  The behaviour of other conversion specifications (and even if other
  character sequences commencing with \code{\%} \emph{are} conversion
  specifications) is system-specific.
#ifdef windows
  For output on Windows, a conversion specification is \code{\%}
  optionally followed by \code{#} and then by a single letter.  Any
  conversion specification which is unimplemented is ignored.
#endif
}
\value{
  The \code{format} methods and \code{strftime} return character vectors
  representing the time.  \code{NA} times are returned as \code{NA_character_}.

  \code{strptime} turns character representations into an object of
  class \code{"\link{POSIXlt}"}.  The time zone is used to set the
  \code{isdst} component and to set the \code{"tzone"} attribute if
  \code{tz != ""}.  If the specified time is invalid (for example
  \samp{"2010-02-30 08:00"}) all the components of the result are
  \code{NA}.  (NB: this does means exactly what it says -- if it is an
  invalid time, not just a time that does not exist in some time zone.)
  %% One of Master B Diggs moments of infamy
}
\note{
  The default formats follow the rules of the ISO 8601 international
  standard which expresses a day as \code{"2001-02-28"} and a time as
  \code{"14:01:02"} using leading zeroes as here.  The ISO form uses no
  space to separate dates and times.

  For \code{strptime} the input string need not specify the date
  completely: it is assumed that unspecified seconds, minutes or hours
  are zero, and an unspecified year, month or day is the current one.

  If the time zone specified is invalid on your system, what happens is
  system-specific but it will probably be ignored.

  OS facilities will probably not print years before 1 CE (aka 1 AD)
  correctly.

  Remember that in most time zones some times do not occur and some occur
  twice because of transitions to/from \sQuote{daylight saving} (also
  known a \sQuote{summer}) time.  \code{strptime} does not validate such
  times (it does not assume a specific time zone), but conversion by
  \code{\link{as.POSIXct}}) will do so.  Conversion by \code{strftime}
  and formatting/printing uses OS facilities and may (and does on
  Windows) return nonsensical results for non-existent times at DST
  transitions.
}
\references{
  International Organization for Standardization (2004, 2000, 1988, 1997,
  \dots)
  \emph{ISO 8601.  Data elements and interchange formats --
    Information interchange -- Representation of dates and times.}
  For links to versions available on-line see (at the time of writing)
  \url{http://dotat.at/tmp/ISO_8601-2004_E.pdf} abd
  \url{http://www.qsl.net/g1smd/isopdf.htm}; for information on the
  current official version, see
  \url{http://www.iso.org/iso/en/prods-services/popstds/datesandtime.html}.

  The POSIX 1003.1 standard, which is in some respects stricter than ISO 8601.
}
\seealso{
  \link{DateTimeClasses} for details of the date-time classes;
  \link{locales} to query or set a locale.

  Your system's help pages on \code{strftime} and \code{strptime} to
  see how to specify their formats.

  (On some Unix-like systems \code{strptime} is replaced by corrected
  code from \samp{glibc}, when all the conversion specifications
  described here are supported, but with no alternative number
  representation nor era available in any locale.)

  Windows users will find no help page for \code{strptime}: code based
  on \samp{glibc} is used (with corrections), so all the conversion
  specifications described here are supported, but with no alternative
  number representation nor era available in any locale.
}
\examples{\donttest{
## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y \%Z")

## time to sub-second accuracy (if supported by the OS)
format(Sys.time(), "\%H:\%M:\%OS3")

## read in date info in format 'ddmmmyyyy'
## This will give NA(s) in some locales; setting the C locale
## as in the commented lines will overcome this on most systems.
## lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- strptime(x, "\%d\%b\%Y")
## Sys.setlocale("LC_TIME", lct)
z

## read in date/time info in format 'm/d/y h:m:s'
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
x <- paste(dates, times)
strptime(x, "\%m/\%d/\%y \%H:\%M:\%S")

## time with fractional seconds
z <- strptime("20/2/06 11:16:16.683", "\%d/\%m/\%y \%H:\%M:\%OS")
z # prints without fractional seconds
op <- options(digits.secs = 3)
z
options(op)

## time zones name are not portable, but 'EST5EDT' comes pretty close.
(x <- strptime(c("2006-01-08 10:07:52", "2006-08-07 19:33:02"),
               "\%Y-\%m-\%d \%H:\%M:\%S", tz = "EST5EDT"))
attr(x, "tzone")

## An RFC 822 header (Eastern Canada, during DST)
strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "\%a, \%d \%b \%Y \%H:\%M:\%S \%z")
}}
\keyword{utilities}
\keyword{chron}
