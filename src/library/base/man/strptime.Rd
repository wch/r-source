\name{strptime}
\alias{format.POSIXct}
\alias{format.POSIXlt}
\alias{strftime}
\alias{strptime}
\alias{as.character.POSIXt}
\alias{ISOdatetime}
\alias{ISOdate}
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

strftime(x, format="", usetz = FALSE, \dots)
strptime(x, format)

ISOdatetime(year, month, day, hour, min, sec, tz = "")
ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A timezone specification to be used for the conversion.
    System-specific, but \code{""} is the current time zone, and
    \code{"GMT"} is UTC.}
  \item{format}{A character string.  The default is
    \code{"\%Y-\%m-\%d \%H:\%M:\%S"} if any component has a time
    component which is not midnight, and \code{"\%Y-\%m-\%d"}
    otherwise.}
  \item{\dots}{Further arguments to be passed from or to other methods.}
  \item{usetz}{logical.  Should the timezone be appended to the output?
    This is used in printing time, and as a workaround for problems with
    using \code{"\%Z"} on most Linux systems.}
  \item{year, month, day}{numerical values to specify a day.}
  \item{hour, min, sec}{numerical values for a time within a day.}
}
\details{
  \code{strftime} is an alias for \code{format.POSIXlt}, and
  \code{format.POSIXct} first converts to class \code{"POSIXlt"} by
  calling \code{\link{as.POSIXlt}}. Note that only that conversion
  depends on the time zone.

  The usual vector re-cycling rules are applied to \code{x} and
  \code{format} so the answer will be of length that of the longer of the
  vectors.

  Locale-specific conversions to and from character strings are used
  where appropriate and available.  This affects the names of the days
  and months, the AM/PM indicator (if used) and the separators in
  formats such as \code{\%x} and \code{\%X}.

  The details of the formats are system-specific, but the following are
  defined by the ISO C / POSIX standard for \code{strftime} and are
  likely to be widely available.  Any character in the format string other than
  the \code{\%} escape sequences is interpreted literally
  (and \code{\%\%} gives \code{\%}).
  \describe{
    \item{\code{\%a}}{Abbreviated weekday name in the current locale.}
    \item{\code{\%A}}{Full weekday name in the current locale.}
    \item{\code{\%b}}{Abbreviated month name in the current locale.}
    \item{\code{\%B}}{Full month name in the current locale.}
    \item{\code{\%c}}{Date and time, locale-specific.}
    \item{\code{\%d}}{Day of the month as decimal number (01--31).}
    \item{\code{\%H}}{Hours as decimal number (00--23).}
    \item{\code{\%I}}{Hours as decimal number (01--12).}
    \item{\code{\%j}}{Day of year as decimal number (001--366).}
    \item{\code{\%m}}{Month as decimal number (01--12).}
    \item{\code{\%M}}{Minute as decimal number (00--59).}
    \item{\code{\%p}}{AM/PM indicator in the locale.  Used in
      conjuction with \code{\%I} and \bold{not} with \code{\%H}.}
    \item{\code{\%S}}{Second as decimal number (00--61), allowing for
      up to two leap-seconds.}
    \item{\code{\%U}}{Week of the year as decimal number (00--53) using
      the first Sunday as day 1 of week 1.}
    \item{\code{\%w}}{Weekday as decimal number (0--6, Sunday is 0).}
    \item{\code{\%W}}{Week of the year as decimal number (00--53) using
      the first Monday as day 1 of week 1.}
    \item{\code{\%x}}{Date, locale-specific.}
    \item{\code{\%X}}{Time, locale-specific.}
    \item{\code{\%y}}{Year without century (00--99). If you use this on
      input, which century you get is system-specific.  So don't!  Often
      values up to 69 (or 68) are prefixed by 20 and 70--99 by 19.}
    \item{\code{\%Y}}{Year with century.}
    \item{\code{\%z}}{(output only.) Offset from Greenwich, so
      \code{-0800} is 8 hours west of Greenwich.}
    \item{\code{\%Z}}{(output only.) Time zone as a character
      string (empty if not available).}
  }
  Where leading zeros are shown they will be used on output but are
  optional on input.

  Also defined in the current standards but less widely implemented
  (e.g. not for output on Windows) are
  \describe{
    \item{\code{\%F}}{Equivalent to \%Y-\%m-\%d (the ISO 8601 date
      format).}
    \item{\code{\%g}}{The last two digits of the week-based year
      (see \code{\%V}).}
    \item{\code{\%G}}{The week-based year (see \code{\%V}) as a decimal
      number.}
    \item{\code{\%u}}{Weekday as a decimal number (1--7, Monday is 1).}
    \item{\code{\%V}}{Week of the year as decimal number (00--53).
      If the week (starting on Monday) containing 1 January has four or
      more days in the new year, then it is considered week 1. Otherwise, it
      is the last week of the previous year, and the next week is week 1.}
  }

  Other format specifiers in common use include
  \describe{
    \item{\code{\%D}}{Locale-specific date format such as \code{\%m/\%d/\%y}.}
    \item{\code{\%k}}{The 24-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%l}}{The 12-hour clock time with single digits preceded
      by a blank.}
    \item{\code{\%n}}{Newline on output, arbitrary whitespace on input.}
    \item{\code{\%r}}{The 12-hour clock time (using the locale's AM or PM).}
    \item{\code{\%R}}{Equivalent to \code{\%H:\%M}.}
    \item{\code{\%t}}{Newline on output, arbitrary whitespace on input.}
    \item{\code{\%T}}{Equivalent to \code{\%H:\%M:\%S}.}
  }
  There are also \code{\%O[dHImMSUVwWy]} which may emit numbers in an
  alternative local-dependent format (e.g. roman numerals), and
  \code{\%E[cCyYxX]} which can use an alternative \sQuote{era}
  (e.g. a different religious calendar).  Which of these are supported is
  OS-dependent.

  \code{ISOdatetime} and \code{ISOdate} are convenience wrappers for
  \code{strptime}, that differ only in their defaults and that
  \code{ISOdate} sets a timezone.  (For dates without times is would be
  better to use the \code{"\link{Date}"} class.)
}
\value{
  The \code{format} methods and \code{strftime} return character vectors
  representing the time.

  \code{strptime} turns character representations into an object of
  class \code{"POSIXlt"}.

  \code{ISOdatetime} and \code{ISOdate} return an object of class
  \code{"POSIXct"}.
}
\note{
  The default formats follow the rules of the ISO 8601 international
  standard which expresses a day as \code{"2001-02-28"} and a time as
  \code{"14:01:02"} using leading zeroes as here.  The ISO form uses no
  space to separate dates and times.

  If the date string does not specify the date completely, the returned
  answer may be system-specific.  The most common behaviour is to assume
  that unspecified seconds, minutes or hours are zero, and a missing
  year, month or day is the current one.  If it specifies a date
  incorrectly, reliable implementations will give an error and the date
  is reported as \code{NA}.  Unfortunately some common implementations
  (such as \samp{glibc}) are unreliable and guess at the intended meaning.

  If the timezone specified is invalid on your system, what happens is
  system-specific but it will probably be ignored.

  OS facilities will probably not print years before 1CE (aka 1AD)
  correctly.

  Remember that in most timezones some times do not occur and some occur
  twice because of transitions to/from summer time.
}
\references{
  International Organization for Standardization (1988, 1997, \dots)
  \emph{ISO 8601. Data elements and interchange formats --
    Information interchange -- Representation of dates and times.}
  The 1997 version is available on-line at
  \url{ftp://ftp.qsl.net/pub/g1smd/8601v03.pdf}
}
\seealso{
  \link{DateTimeClasses} for details of the date-time classes;
  \code{\link{locales}} to query or set a locale.

  Your system's help pages on \code{strftime} and \code{strptime} to
  see how to specify their formats.
#ifdef windows
  Windows users will find no help page for \code{strptime}: code based
  on \samp{glibc} is used (with corrections), so all the format
  specifiers described here are supported, but with no alternative
  number representation nor era available in any locale.
#endif
}
\examples{
## locale-specific version of date()
format(Sys.time(), "\%a \%b \%d \%X \%Y \%Z")

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
z <- strptime(x, "\%m/\%d/\%y \%H:\%M:\%S")
z
}
\keyword{utilities}
\keyword{chron}
