% File src/library/base/man/strptime.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
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
\method{format}{POSIXlt}(x, format = "", usetz = FALSE,
       digits = getOption("digits.secs"), \dots)

\method{as.character}{POSIXt}(x, digits = if(inherits(x, "POSIXlt")) 14L else 6L,
             OutDec = ".", \dots)

strftime(x, format = "", tz = "", usetz = FALSE, \dots)
strptime(x, format, tz = "")
}
\arguments{
  \item{x}{an object to be converted: a character vector for
    \code{strptime}, an object which can be converted to
    \code{"\link{POSIXlt}"} for \code{strftime}.}
  \item{tz}{a character string specifying the time zone to be used for
    the conversion.  System-specific (see \code{\link{as.POSIXlt}}), but
    \code{""} is the current time zone, and \code{"GMT"} is UTC.
    Invalid values are most commonly treated as UTC, on some platforms with
    a warning.}
  \item{format}{a character string.  The default for the \code{format}
    methods is
    \code{"\%Y-\%m-\%d \%H:\%M:\%S"} if any element has a time
    component which is not midnight, and \code{"\%Y-\%m-\%d"}
    otherwise.  In the first case and if \code{digits} is not \code{NULL},
    i.e., by default when \code{\link{options}("digits.secs")} is set, up to
    the specified number of digits will be printed for seconds, using
    \code{"\%OS<n>"} instead of \code{"\%S"} in the format, see also
    \sQuote{Details}.}
  \item{\dots}{further arguments to be passed from or to other methods; for
    \code{strftime()}, notably \code{digits}, passed to \code{format(<POSIXlt>)}.}
  \item{usetz}{logical.  Should the time zone abbreviation be appended
    to the output?  This is used in printing times, and more reliable
    than using \code{"\%Z"}.}
  \item{digits}{integer determining the \I{\code{format()}ing} of seconds when
    needed.  Note that the defaults for \code{format()} and
    \code{as.character()} differ on purpose, \code{as.character()} giving
    close to full accuracy as it does for numbers.}
  \item{OutDec}{a 1-character string specifying the decimal point to be
    used; the default is \emph{not} \code{\link{getOption}("OutDec")} on
    purpose.}
}
\details{
  The \code{format} and \code{as.character} methods and \code{strftime}
  convert objects from the classes \code{"\link{POSIXlt}"} and
  \code{"\link{POSIXct}"} to \code{\link{character}} vectors.

  \code{strptime} converts character vectors to class \code{"POSIXlt"}:
  its input \code{x} is first converted by \code{\link{as.character}}.
  Each input string is processed as far as necessary for the format
  specified: any trailing characters are ignored.

  \code{strftime} is a wrapper for \code{format.POSIXlt}, and it and
  \code{format.POSIXct} first convert to class \code{"POSIXlt"} by
  calling \code{\link{as.POSIXlt}} (so they also work for class
  \code{"\link{Date}"}).  Note that only that conversion depends on the
  time zone.  Since \R version 4.2.0, \code{as.POSIXlt()} conversion now
  treats the non-finite numeric \code{-Inf}, \code{Inf}, \code{NA} and
  \code{NaN} differently (where previously all were treated as
  \code{NA}). Also the \code{format()} method for \code{POSIXlt} now
  treats these different non-finite times and dates analogously to type
  \code{\link{double}}.

  The usual vector re-cycling rules are applied to \code{x} and
  \code{format} so the answer will be of length of the longer of these
  vectors.

  Locale-specific conversions to and from character strings are used
  where appropriate and available.  This affects the names of the days
  and months, the AM/PM indicator (if used) and the separators in output
  formats such as \code{\%x} and \code{\%X}, \emph{via} the setting of
  the \code{\link{LC_TIME}} locale category.  The \sQuote{current
  locale} of the descriptions might mean the locale in use at the start
  of the \R session or when these functions are first used.  (For input,
  the locale-specific conversions can be changed by calling
  \code{\link{Sys.setlocale}} with category \code{LC_TIME} (or
  \code{LC_ALL}).  For output, what happens depends on the OS but
  usually works.)

  % strftime is C99: strptime is POSIX.
  The details of the formats are platform-specific, but the following are
  likely to be widely available: most are defined by the POSIX standard.
  A \emph{conversion specification} is introduced by \code{\%}, usually
  followed by a single letter or \code{O} or \code{E} and then a single
  letter.  Any character in the format string not part of a conversion
  specification is interpreted literally (and \code{\%\%} gives
  \code{\%}).  Widely implemented conversion specifications include
  \describe{
    \item{\code{\%a}}{Abbreviated weekday name in the current
      locale on this platform.  (Also matches full name on input:
      in some locales there are no abbreviations of names.)
      Case-insensitive on input.}
    \item{\code{\%A}}{Full weekday name in the current locale.  (Also
      matches abbreviated name on input.)
      Case-insensitive on input.}
    \item{\code{\%b}}{Abbreviated month name in the current locale on
      this platform.  (Also matches full name on input: in
      some locales there are no abbreviations of names.)
    Case-insensitive on input.}
    \item{\code{\%B}}{Full month name in the current locale.  (Also
      matches abbreviated name on input.)
      Case-insensitive on input.}
    \item{\code{\%c}}{Date and time.   Locale-specific on output,
      \code{"\%a \%b \%e \%H:\%M:\%S \%Y"} on input.}
    \item{\code{\%C}}{Century (00--99): the integer part of the year
      divided by 100.}
    \item{\code{\%d}}{Day of the month as decimal number (01--31).}
    \item{\code{\%D}}{Date format such as \code{\%m/\%d/\%y}: the C99
      standard says it should be that exact format (but not all OSes
      comply).}
    \item{\code{\%e}}{Day of the month as decimal number (1--31), with
      a leading space for a single-digit number.}
    \item{\code{\%F}}{Equivalent to \%Y-\%m-\%d (the ISO 8601 date
      format).}
    \item{\code{\%g}}{The last two digits of the week-based year
      (see \code{\%V}).  (Accepted but ignored on input.)}
    \item{\code{\%G}}{The week-based year (see \code{\%V}) as a decimal
      number.  (Accepted but ignored on input.)}
    \item{\code{\%h}}{Equivalent to \code{\%b}.}
    \item{\code{\%H}}{Hours as decimal number (00--23).  As a special
      exception strings such as \samp{24:00:00} are accepted for input,
      since ISO 8601 allows these.}
    \item{\code{\%I}}{Hours as decimal number (01--12).}
    \item{\code{\%j}}{Day of year as decimal number (001--366):  For
      input, 366 is only valid in a leap year.}
    \item{\code{\%m}}{Month as decimal number (01--12).}
    \item{\code{\%M}}{Minute as decimal number (00--59).}
    \item{\code{\%n}}{Newline on output, arbitrary whitespace on input.}
    \item{\code{\%p}}{AM/PM indicator in the locale.  Used in
      conjunction with \code{\%I} and \bold{not} with \code{\%H}.  An
      empty string in some locales (for example on some OSes,
      non-English European locales including Russia). The behaviour is
      undefined if used for input in such a locale.

      For input the match is case-insensitive.  Note that
      English locales do not necessarily use \code{AM/PM}: for example,
      \code{a.m./p.m.} has been encountered.

      Some platforms accept \code{\%P} for output, which uses a lower-case
      version (\code{\%p} may also use lower case): others will output
      \code{P}.}
    \item{\code{\%r}}{For output, the 12-hour clock time (using the
      locale's AM or PM): only defined in some locales, and on some OSes
      misleading in locales which do not define an AM/PM indicator.
      For input, equivalent to \code{\%I:\%M:\%S \%p}.}
    \item{\code{\%R}}{Equivalent to \code{\%H:\%M}.}
    \item{\code{\%S}}{Second as integer (00--61), allowing for
      up to two leap-seconds (but POSIX-compliant implementations
      will ignore leap seconds).}
    \item{\code{\%t}}{Tab on output, arbitrary whitespace on input.}
    \item{\code{\%T}}{Equivalent to \code{\%H:\%M:\%S}.}
    \item{\code{\%u}}{Weekday as a decimal number (1--7, Monday is 1).}
    % see https://en.wikipedia.org/wiki/Week_number#Week_number
    \item{\code{\%U}}{Week of the year as decimal number (00--53) using
      Sunday as the first day 1 of the week (and typically with the
      first Sunday of the year as day 1 of week 1).  The US convention.}
    \item{\code{\%V}}{Week of the year as decimal number (01--53) as
      defined in ISO 8601.
      If the week (starting on Monday) containing 1 January has four or
      more days in the new year, then it is considered week 1.  Otherwise, it
      is the last week of the previous year, and the next week is week
      1. See \code{\%G} (\code{\%g}) for the year corresponding to the
      week given by \code{\%V}. (Accepted but ignored on input.)}
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
      behaviour specified by the 2018 POSIX standard, but it does
      also say \sQuote{it is expected that in a future version the
	default century inferred from a 2-digit year will change}.}
    \item{\code{\%Y}}{Year with century.  Note that whereas there was no
      zero in the original Gregorian calendar, ISO 8601:2004 defines it
      to be valid (interpreted as 1BC): see
      \url{https://en.wikipedia.org/wiki/0_(year)}.  However, the standards
      also say that years before 1582 in its calendar should only be used
      with agreement of the parties involved.

      For input, only years \code{0:9999} are accepted.}
    \item{\code{\%z}}{Signed offset in hours and minutes from UTC, so
      \code{-0800} is 8 hours behind UTC.  (Standard only for output.  For
      input \R currently supports it on all platforms -- values from
      \code{-1400} to \code{+1400} are accepted.)}
    %% Nowehere currently uses less than -1200 nor more than 1400.
    \item{\code{\%Z}}{(Output only.)  Time zone abbreviation as a
      character string (empty if not available).  This may not be reliable
      when a time zone has changed abbreviations over the years.}
  }
  Where leading zeros are shown they will be used on output but are
  optional on input.  Names are matched case-insensitively on input:
  whether they are capitalized on output depends on the platform and the
  locale.  Note that abbreviated names are platform-specific (although
  the standards specify that in the \samp{C} locale they must be the
  first three letters of the capitalized English name: this convention
  is widely used in English-language locales but for example the French
  month abbreviations are not the same on any two of Linux, macOS, Solaris
  and Windows). Knowing what the abbreviations are is essential
  if you wish to use \code{\%a}, \code{\%b} or \code{\%h} as part of an
  input format: see the examples for how to check.

  When \code{\%z} or \code{\%Z} is used for output with an
  object with an assigned time zone an attempt is made to use the values
  for that time zone --- but it is not guaranteed to succeed.

  The definition of \sQuote{whitespace} for \code{\%n} and \code{\%t}
  is platform-dependent: for most it does not include non-breaking spaces.

  Not in the standards and less widely implemented are
  \describe{
    \item{\code{\%k}}{The 24-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%l}}{The 12-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%s}}{(Output only.) The number of seconds since the
      epoch.}
    \item{\code{\%+}}{(Output only.) Similar to \code{\%c}, often
      \code{"\%a \%b \%e \%H:\%M:\%S \%Z \%Y"}. May depend on the locale.}
  }
  For output there are also \code{\%O[dHImMUVwWy]} which may emit
  numbers in an alternative locale-dependent format (e.g., roman
  numerals), and \code{\%E[cCyYxX]} which can use an alternative
  \sQuote{era} (e.g., a different religious calendar).  Which of these
  are supported is OS-dependent.  These are accepted for input, but with
  the standard interpretation.

  Specific to \R is \code{\%OSn}, which for output gives the seconds
  truncated to exactly \code{0 <= n <= 6} decimal places.  If \code{\%OS} is
  not followed by a digit, it uses up to \code{digits} decimals (only as
  many as necessary, i.e., not producing extra trailing zeros.
  The default \code{digits = NULL} corresponds to \code{digits = 0}.

  Note that the precedence is \code{format=".. \%OSn .."} \eqn{\ll}{>>}
  \code{digits = n}, the latter initialized by default from
  \code{getOption("digits.secs")}.
  Currently, for output (\code{strftime()} and \code{format(<POSIX.t>)}),
  only \emph{one} "\%OS" occurrence is handled (per format string).
  \cr
  Further, for \code{strptime} \code{\%OS} will input seconds including
  fractional seconds (to full precision).  Note that \code{\%S} does not
  read or write fractional parts.

  The behaviour of other conversion specifications (and even if other
  character sequences commencing with \code{\%} \emph{are} conversion
  specifications) is system-specific.  Some systems document that the
  use of multi-byte characters in \code{format} is unsupported: UTF-8
  locales are unlikely to cause a problem.
}
\value{
  The \code{format} methods and \code{strftime} return character vectors
  representing the time.  \code{NA} times are returned as
  \code{NA_character_}.

  \code{strptime} turns character representations into an object of
  class \code{"\link{POSIXlt}"}.  The time zone is used to set the
  \code{isdst} component and to set the \code{"tzone"} attribute if
  \code{tz != ""}.  If the specified time is invalid (for example
  \samp{"2010-02-30 08:00"}) all the components of the result are
  \code{NA}.  (NB: this does means exactly what it says -- if it is an
  invalid time, not just a time that does not exist in some time zone.)
}

\section{Printing years}{
  Everyone agrees that years from 1000 to 9999 should be printed with 4
  digits, but the standards do not define what is to be done outside
  that range.  For years 0 to 999 most OSes pad with zeros or spaces to
  4 characters, but Linux/\I{glibc} outputs just the number.

  OS facilities will probably not print years before 1 CE (aka 1 AD)
  \sQuote{correctly} (they tend to assume the existence of a year 0: see
  \url{https://en.wikipedia.org/wiki/0_(year)}, and some OSes get them
  completely wrong).  Common formats are \code{-45} and \code{-045}.

  Years after 9999 and before -999 are normally printed with five or
  more characters.

  Some platforms support modifiers from POSIX 2008 (and others).  On
  Linux/\I{glibc} the format \code{"\%04Y"} assures a minimum of four
  characters and zero-padding (the default is no padding).  The internal
  code (as used on Windows and by default on macOS) uses zero-padding by
  default (this can be controlled by environment variable
  \env{R_PAD_YEARS_BY_ZERO}).  On those platforms, formats \code{\%04Y},
  \code{\%_4Y} and \code{\%_Y} can be used for zero, space and no
  padding respectively. (On macOS, the native code (not the default)
  supports none of these and uses zero-padding to 4 digits.)
}

\section{Time zone offsets}{
  Offsets from GMT (also known as UTC) are part of the conversion
  between timezones and to/from class \code{"POSIXct"}, but cause
  difficulties as they are often computed incorrectly.

  They conventionally have the opposite sign from time-zone
  specifications (see \code{\link{Sys.timezone}}): positive values are
  East of the meridian.  Although there have been time zones with
  offsets like +00:09:21 (Paris in 1900), and -00:44:30 (Liberia until
  1972), offsets are usually treated as whole numbers of minutes, and
  are most often seen in RFC 5322 email headers in forms like
  \code{-0800} (e.g., used on the Pacific coast of the USA in winter).

  Format \code{\%z} can be used for input or output: it is a character
  string, conventionally plus or minus followed by two digits for hours
  and two for minutes: the standards say that an empty string should be
  output if the offset is undetermined, but some systems use
  \code{+0000} or the offsets for the time zone in use for the current
  year.  (On some platforms this works better after conversion to
  \code{"POSIXct"}.  Some platforms only recognize hour or half-hour
  offsets for output.)%% strftime in macOS 13.

  Using \code{\%z} for input makes most sense with \code{tz = "UTC"}.
}

\note{
  The default formats follow the rules of the ISO 8601 international
  standard which expresses a day as \code{"2001-02-28"} and a time as
  \code{"14:01:02"} using leading zeroes as here.  (The ISO form uses no
  space, possibly \samp{T}, to separate dates and times: \R uses a space
  by default.)

  For \code{strptime} the input string need not specify the date
  completely: it is assumed that unspecified seconds, minutes or hours
  are zero, and an unspecified year, month or day is the current one.
  (However, if a month is specified, the day of that month has to be
  specified by \code{\%d} or \code{\%e} since the current day of the
  month need not be valid for the specified month.)  Some components may
  be returned as \code{NA} (but an unknown \code{tzone} component is
  represented by an empty string).

  If the time zone specified is invalid on your system, what happens is
  system-specific but it will probably be ignored.

  Remember that in most time zones some times do not occur and some
  occur twice because of transitions to/from \sQuote{daylight saving}
  (also known as \sQuote{summer}) time.  \code{strptime} does not
  validate such times (it does not assume a specific time zone), but
  conversion by \code{\link{as.POSIXct}} will do so.  Conversion by
  \code{strftime} and formatting/printing uses OS facilities and may
  return nonsensical results for non-existent times at DST transitions.

  In a C locale \code{\%c} is required to be
  \code{"\%a \%b \%e \%H:\%M:\%S \%Y"}.  As Windows does not comply (and
  uses a date format not understood outside N. America), that format is
  used by \R on Windows in all locales.

  There is a limit of 2048 bytes on each string produced by
  \code{strftime} and the \code{format} methods.  As from \R 4.3.0
  attempting to exceed this is an error (previous versions silently
  truncated at 255 bytes).
}

\section{Sources}{
  Input uses the POSIX function \code{strptime} and output the C99
  function \code{strftime}.

  However, not all OSes (notably Windows) provided \code{strptime} and
  many issues were found for those which did, so since 2000 \R has used
  a fork of code from \samp{glibc}.  The forked code uses the
  system's \code{strftime} to find the locale-specific day and month
  names and any AM/PM indicator.

  On some platforms (including Windows and by default on macOS) the
  system's \code{strftime} is replaced (along with most of the rest of
  the C-level datetime code) by code modified from \abbr{IANA}'s \samp{tzcode}
  distribution (\url{https://www.iana.org/time-zones}).

  Note that as \code{strftime} is used for output (and not
  \code{wcsftime}), argument \code{format} is translated if necessary to
  the session encoding.
}

\references{
  International Organization for Standardization (2004, 2000, \dots)
  \sQuote{ISO 8601.  Data elements and interchange formats --
    Information interchange -- Representation of dates and times.},
  slightly updated to International Organization for Standardization (2019)
  \sQuote{ISO 8601-1:2019.  Date and time -- Representations for
    information interchange -- Part 1: Basic rules}, and further amended
  in 2022.
  For links to versions available on-line see (at the time of writing)
  \url{https://dotat.at/tmp/ISO_8601-2004_E.pdf} and
  \url{https://www.qsl.net/g1smd/isopdf.htm}; for information on the
  current official version, see \url{https://www.iso.org/iso/iso8601} and
  \url{https://en.wikipedia.org/wiki/ISO_8601}.

  The POSIX 1003.1 standard, which is in some respects stricter than ISO 8601.
}
\seealso{
  \link{DateTimeClasses} for details of the date-time classes;
  \link{locales} to query or set a locale.

  Your system's help page on \code{strftime} to see how to specify their
  formats.  (On some systems, including Windows, \code{strftime} is
  replaced by more comprehensive internal code.)
}
\examples{\donttest{
## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y \%Z")

## time to sub-second accuracy (if supported by the OS)
format(Sys.time(), "\%H:\%M:\%OS3")
}% \dont.. %
## read in date info in format 'ddmmmyyyy'
## This will give NA(s) in some locales (especially non-English ones);
## setting the C locale as in the commented lines will overcome this on
## most systems.
## lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- strptime(x, "\%d\%b\%Y")
## Sys.setlocale("LC_TIME", lct)
\donttest{z}
(chz <- as.character(z)) # same w/o TZ
## *here* (but not in general), the same as format():
stopifnot(exprs = {
     identical(chz, format(z))
     grepl("^1960-0[137]-[03][012]$", chz[!is.na(z)])
})

## read in date/time info in format 'm/d/y h:m:s'
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
x <- paste(dates, times)
z2 <- strptime(x, "\%m/\%d/\%y \%H:\%M:\%S")
\donttest{z2 }
## *here* (but not in general), the same as format():
stopifnot(identical(format(z2), as.character(z2)))

## time with fractional seconds (setting `tz = ..` for reproducible output)
z3 <- strptime("20/2/06 11:16:16.683", "\%d/\%m/\%y \%H:\%M:\%OS", tz = "UTC")
z3 # prints without fractional seconds by default, digits.sec = NULL ("= 0")
format(z3, digits = 3) # shows extra digits
format(z3, digits = 6) # still 3 digits: *not* showing trailing zeros
format(z3, format = "\%Y-\%m-\%d \%H:\%M:\%OS6") #  *does* keep trailing zeros
op <- options(digits.secs = 3) # global option, the default for `digits`
z3 # shows the 3 extra digits
options(op)
as.character(z3) # ditto


## time zone names are not portable, but 'EST5EDT' comes pretty close.
## (but its interpretation may not be universal: see ?timezones)
z4 <- strptime(c("2006-01-08 10:07:52", "2006-08-07 19:33:02"),
               "\%Y-\%m-\%d \%H:\%M:\%S", tz = "EST5EDT")
\donttest{z4 }
attr(z4, "tzone")
as.character(z4)
z4$sec[2] <- pi # "very" fractional seconds
as.character(z4) # shows full precision
format(z4) # no fractional sec
format(z4, digits=8) # shows only 6  (hard-wired maximum)
format(z4, digits=4)


## An RFC 5322 header (Eastern Canada, during DST)
## In a non-English locale the commented lines may be needed.
\donttest{
## prev <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
strptime("Tue, 23 Mar 2010 14:36:38 -0400", "\%a, \%d \%b \%Y \%H:\%M:\%S \%z")
## Sys.setlocale("LC_TIME", prev)

## Make sure you know what the abbreviated names are for you if you wish
## to use them for input (they are matched case-insensitively):
format(s1 <- seq.Date(as.Date('1978-01-01'), by = 'day',   len =  7), "\%a")
format(s2 <- seq.Date(as.Date('2000-01-01'), by = 'month', len = 12), "\%b")
}% dont %
## Non-finite date-times :
format(as.POSIXct(Inf)) # "Inf"  (was  NA  in R <= 4.1.x)
format(as.POSIXlt(c(-Inf,Inf,NaN,NA))) # were all NA
}
\keyword{utilities}
\keyword{chron}
