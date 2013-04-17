% File src/library/base/man/timezones.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{timezones}
\alias{Sys.timezone}
\alias{timezone}
\alias{time zone}
\alias{time zones}
\alias{TZ}
#ifdef windows
\alias{TZDIR}
#endif

\title{Time Zones}
\description{
  Information about time zones in \R.  \code{Sys.timezone} returns
  the current time zone.
}
\usage{
Sys.timezone()
}

\details{
  Time zones are a system-specific topic, but these days almost all \R
  platforms use the same underlying code, used by Linux, OS X,
  Solaris, AIX, FreeBSD, Sun Java >= 1.4 and Tcl >= 8.5, and installed
  with \R on Windows.

  It is not in general possible to retrieve the system's own name(s) for
  the current timezone, but \code{Sys.timezone} will retrieve the name
  it uses for the current time (and the name may differ depending on
  whether daylight saving time is in effect).

  On most platforms it is  possible to set the time zone via the
  environment variable \env{TZ}: see the section on \sQuote{Time zone
    names} for suitable values.

  Note that the principal difficulty with time zones is their individual
  history: over the last 100 years places have changed their affiliation
  between major time zones, have opted out of (or in to) DST in
  various years or adopted rule changes late or not at all.  This often
  involves tiny administrative units in the US/Canada: Iowa had 23
  different implementations of DST in the 1960's!

  Time zones did not come into use until the second half of the
  nineteenth century, and DST was first introduced in the early
  twentieth century, most widely during the First World War (in 1916).
#ifdef unix
  The most common implementation of \code{POSIXct} is as signed 32-bit
  integers and so only goes back to the end of 1901: on such systems \R
  assumes that dates prior to that are in the same time zone as they
  were in 1902.
#endif
}

\note{
  There is currently (since 2007) considerable disruption over changes
  to the timings of the DST transitions, aimed at energy conservation.
  These often have short notice and timezone databases may not be up to
  date (even if the OS has been updated recently).

  Note that except on Windows, the operation of time zones is an OS
  service, and even on Windows a third-party database is used and can be
  updated (see the section on \sQuote{Time zone names}).  Incorrect
  results will never be an \R issue, so please ensure that you have the
  courtesy not to blame \R for them.
}
% Unlike Tony Plate.

\value{
  \code{Sys.timezone} returns an OS-specific character string, possibly
  an empty string.  Typically this is an abbreviation such as \code{"EST"}.
}

\section{Time zone names}{
  Where OSes describe their valid time zones can be obscure.  The help
  for the C function \code{tzset} can be helpful, but it
  can also be inaccurate.  There is a cumbersome POSIX specification
  (listed under environment variable \env{TZ} at
  \url{http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08}),
  which is often at least partially supported, but there usually are
  other more user-friendly ways to specify timezones.

  Many systems make use of a timezone database compiled by Arthur Olson,
  in which the preferred way to refer to a time zone by a location
  (typically of a city) e.g. \code{Europe/London},
  \code{America/Los_Angeles}, \code{Pacific/Easter}.  Some traditional
  designations are also allowed such as \code{EST5EDT} or
  \code{GB}.  (Beware that some of these designations may not be what you
  think: in particular \code{EST} is a time zone used in Canada
  \emph{without} daylight savings time, and not \code{EST5EDT} nor
  (Australian) Eastern Standard Time.)  The designation can also be an
  optional colon prepended to the path to a file giving complied zone
  information (and the examples above are all files in a system-specific
  location).  See \url{http://www.twinsun.com/tz/tz-link.htm} for more
  details and references.  By convention, regions with a unique timezone
  history since 1970 have specific names, but those with different
  earlier histories may not.
#ifdef unix

  For most Unix-alikes use the Olson database.  The system-specific
  default location in the file system varies, e.g.
  \file{/usr/share/zoneinfo} (Linux, OS X, FreeBSD),
  \file{/usr/share/lib/zoneinfo} (Solaris, AIX), \file{/usr/etc/zoneinfo},
  \ldots.  It is likely that there is a file \file{zone.tab} in that
  directory listing the locations known as time-zone names (but not for
  example \code{EST5EDT}).  See also
  \url{http://en.wikipedia.org/wiki/Zone.tab}.
#endif
#ifdef windows

  \R under Windows uses the Olson database.  The current version of the
  database will be given in file
  \file{\var{\link{R_HOME}}\\share\\zoneinfo\\VERSION}.
  Environment variable \env{TZDIR} can be used to point to a later
  \file{zoneinfo} directory.  A file listing most known time zones can be
  found at \file{\var{\link{R_HOME}}\\share\\zoneinfo\\zone.tab} (see
  \sQuote{Examples}).   By convention, regions with a unique timezone
  history since 1970 have specific names, but those with different
  earlier histories may not.

  An attempt is made (once only per session) to map Windows' idea of
  the current time zone to a location, following an earlier version of
  \url{http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml}
  If this is not successful, it can be overridden by setting the
  \env{TZ} environment variable.

  Windows documents a specification of the form \code{GST-1GDT}: this is
  interpreted as POSIX-like and hence the \sQuote{US rules} for changing
  to/from DST are applied (and are incorrect for Germany).  Versions of
  \R prior to 2.7.0 used Windows' system functions and hence this form:
  it is still accepted for backwards compatibility but was (and remains)
  unreliable and gives a warning.
#endif

  Many systems support timezones of the form \samp{GMT+n} and
  \samp{GMT-n}, which are at a fixed offset from UTC (hence no DST).
  Contrary to some usage (but consistent with names such as
  \samp{PST8PDT}), negative offsets are times ahead of (east of) UTC,
  positive offsets are times behind (west of) UTC.
}
\seealso{
  \code{\link{Sys.time}}, \code{\link{as.POSIXlt}}.

  \url{http://en.wikipedia.org/wiki/Time_zone} and
  \url{http://www.twinsun.com/tz/tz-link.htm} for extensive sets of links.
}

\examples{
Sys.timezone()

#ifdef windows
tzfile <- file.path(R.home("share"), "zoneinfo", "zone.tab")
tzones <- read.delim(tzfile, row.names = NULL, header = FALSE,
    col.names = c("country", "coords", "name", "comments"),
    as.is = TRUE, fill = TRUE, comment.char = "#")
str(tzones$name)
#endif
#ifdef unix
\dontrun{
## need to supply a suitable file path (if any) for your system
tzfile <- "/usr/share/zoneinfo/zone.tab"
tzones <- read.delim(tzfile, row.names = NULL, header = FALSE,
    col.names = c("country", "coords", "name", "comments"),
    as.is = TRUE, fill = TRUE, comment.char = "#")
str(tzones$name)
}
#endif
}
\keyword{utilities}
\keyword{chron}
