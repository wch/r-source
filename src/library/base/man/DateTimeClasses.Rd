\name{DateTimeClasses}
% implementation mostly in ../R/datetime.R 
\alias{POSIXct}
\alias{POSIXlt}
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
\alias{as.data.frame.POSIXct}
\alias{.leap.seconds}
\alias{is.na.POSIXlt}
\alias{all.equal.POSIXct}
\alias{c.POSIXct}
\alias{c.POSIXlt}
\alias{as.matrix.POSIXlt}
\alias{mean.POSIXct}
\alias{mean.POSIXlt}
\alias{str.POSIXt}

% back-compatibility functions
\alias{-.POSIXct}
\alias{-.POSIXlt}
\alias{Ops.POSIXct}
\alias{Ops.POSIXlt}

\title{Date-Time Classes}
\description{
  Description of the classes \code{"POSIXlt"} and \code{"POSIXct"}
  representing calendar dates and times (to the nearest second). 
}
\usage{
print.POSIXct(x, ...)
print.POSIXlt(x, ...)

summary.POSIXct(object, digits = 15, ...)
summary.POSIXlt(object, digits = 15, ...)

time + number
time - number
time1 lop time2

c(time1, ..., recursive = FALSE)
mean(time)
}
\arguments{
  \item{x, object}{An object to be printed or summarized.}
  \item{digits}{Number of significant digits for the computations:
    should be high enough to represent the least important time unit
    exactly.}
  \item{\dots}{Further arguments to be passed from or to other methods.}
  \item{time, time1, time2, \dots}{date-time objects.}
  \item{number}{a numeric object.}
  \item{lop}{One of \code{==}, \code{!=}, \code{<}, \code{<=}, \code{>}
    or \code{>=}.}
}
\details{
  There are two basic classes of date/times.  Class \code{"POSIXct"}
  represents the (signed) number of seconds since the beginning of 1970
  as a numeric vector.  Class \code{"POSIXlt"} is a named list of
  vectors representing
  \describe{
    \item{\code{sec}}{0--61: seconds}
    \item{\code{min}}{0--59: minutes}
    \item{\code{hour}}{0--23: hours}
    \item{\code{mday}}{1--31: day of the month}
    \item{\code{mon}}{0--11: months after the first of the year.}
    \item{\code{year}}{Years since 1900.}
    \item{\code{wday}}{0--6 day of the week, starting on Sunday.}
    \item{\code{yday}}{0--365: day of the year.}
    \item{\code{isdst}}{Daylight savings time flag. Positive if in
      force, zero if not, negative if unknown.}
  }
  The classes correspond to the ANSI C constructs of ``calendar time''
  (the \code{time\_t} data type) and ``local time'' (or broken-down
  time, the \code{struct tm} data type), from which they also inherit
  their names.

  \code{"POSIXct"} is more convenient for including in data frames, and
  \code{"POSIXlt"} is closer to human-readable forms.
  A virtual class \code{"POSIXt"} inherits from both of the classes: it
  is used to allow operations such as subtraction to mix the two classes.

  Logical comparisons and limited arithmetic are available for both classes.
  One can add or subtract a number of seconds or a
  \code{\link{difftime}} object from a date-time object,
  but not add two date-time objects.  Subtraction of two date-time objects
  is equivalent to using \code{\link{difftime}}.  Be aware
  that \code{"POSIXlt"} objects will be interpreted as being in the
  current timezone for these operations, unless a timezone has been
  specified.

  \code{"POSIXlt"} objects will often have an attribute \code{"tzone"},
  a character vector of length 3 giving the timezone name from the
  \code{"TZ"} environment variable and the names of the base timezone
  and the alternate (daylight-saving) timezone.  Sometimes this may
  just be of length one, giving the timezone name.

  Unfortunately, the conversion is complicated by the operation of time
  zones and leap seconds (22 days have been 86401 seconds long so far:
  the times of the extra seconds are in the object \code{.leap.seconds}).
  The details of this are entrusted to the OS services where possible.
  This will usually cover the period 1970--2037, and on Unix machines
  back to 1902 (when time zones were in their infancy).  Outside those
  ranges we use our own C code.  This uses the offset from GMT in use
  in the timezone in 2000, and uses the alternate (daylight-saving)
  timezone only if \code{isdst} is positive.

  It seems that some systems use leap seconds but most do not. This is
  detected and corrected for at build time, so all \code{"POSIXct"}
  times used by \R do not include leap seconds.  (Conceivably this could
  be wrong if the system has changed since build time, just possibly by
  changing locales.)

  Using \code{c} on \code{"POSIXlt"} objects converts them to the
  current time zone.

}
\seealso{
  \code{\link{as.POSIXct}} and \code{\link{as.POSIXlt}} for conversion
  between the classes.

  \code{\link{strptime}} for conversion to and from character
  representations.

  \code{\link{Sys.time}} for clock time as a \code{"POSIXct"} object.

  \code{\link{difftime}} for time intervals.

  \code{\link{cut.POSIXt}}, \code{\link{seq.POSIXt}},
  \code{\link{round.POSIXt}} and \code{\link{trunc.POSIXt}} for methods
  for these classes.

  \code{weekday.POSIXt} for convenience extraction functions.
}
\section{Warning}{
  Some Unix-like systems (especially Linux ones) do not have \code{"TZ"}
  set, yet have internal code that expects it (as does POSIX).  We have
  tried to work around this, but if you get unexpected results try
  setting \code{"TZ"}.
}
\examples{
(z <- Sys.time())             # the current date, as class "POSIXct"

Sys.time() - 3600             # an hour ago

as.POSIXlt(Sys.time(), "GMT") # the current time in GMT
format(.leap.seconds)         # all 22 leapseconds in your timezone

%% `advanced' examples:
\testonly{
str(.leap.seconds)

## Wouldn't we want a  diff.POSIXt()  method ?
(dls <- .leap.seconds[-1] - .leap.seconds[-22])
table(dls)
}
}
\keyword{utilities}
\keyword{chron}
