% File src/library/base/man/timezones.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{timezones}
\alias{Sys.timezone}
\alias{OlsonNames}
\alias{timezone}
\alias{timezones}
\alias{time zone}
\alias{time zones}
\alias{TZ}
\alias{TZDIR}

\title{Time Zones}
\description{
  Information about time zones in \R.  \code{Sys.timezone} returns
  the name of the current time zone.
}

\usage{
Sys.timezone(location = TRUE)

OlsonNames()
}

\arguments{
  \item{location}{logical: should an attempt be made to find the
    location name as used in the Olson/IANA database?
    (See \sQuote{Time zone names} below.)}
}

\details{
  Time zones are a system-specific topic, but these days almost all \R
  platforms use similar underlying code, used by Linux, OS X, Solaris,
  AIX, FreeBSD, Sun Java >= 1.4 and Tcl >= 8.5, and installed with \R on
  Windows.  Unfortunately there are many system-specific errors in the
  implementations.  It is possible to use \R's own version of the code
  on Unix-alikes as well as on Windows: this is the default for OS X and
  recommended for Solaris.

  It should be possible to set the time zone via the environment
  variable \env{TZ}: see the section on \sQuote{Time zone names} for
  suitable values.  \code{Sys.timezone()} will return the value of
  \env{TZ} if set (and on some OSes it is always set), otherwise it will
  try to retrieve a value which if set for \env{TZ} would give the
  current time zone.  This is not in general possible, and
  \code{Sys.timezone(FALSE)} on Windows will retrieve the abbreviation
  used for the current time.

  If \env{TZ} is set but empty or invalid, most platforms default to
  \samp{UTC}, the time zone colloquially known as \samp{GMT} (see
  \url{http://en.wikipedia.org/wiki/Coordinated_Universal_Time}).  (Some
  but not all platforms will give a warning for invalid timezones.)

  Time zones did not come into use until the second half of the
  nineteenth century and were not widely adopted until the twentieth,
  and \emph{daylight saving time} (DST, also known as \emph{summer
  time}) was first introduced in the early twentieth century, most
  widely in 1916. Over the last 100 years places have changed their
  affiliation between major time zones, have opted out of (or in to) DST
  in various years or adopted DST rule changes late or not at all.
  
  The most common system implementation of \code{POSIXct} is as signed
  32-bit integers and so only goes back to the end of 1901: on such
  systems \R assumes that dates prior to that are in the same time zone
  as they were in 1902.  Most of the world had not adopted time zones by
  1902 (so used local \sQuote{mean time} based on longitude) but for
  some places there had been time-zone changes before then.  64-bit
  representations are becoming common; unfortunately on some 64-bit OSes
  the database information is only available for the range 1901--2038.
}

\value{
  \code{Sys.timezone} returns an OS-specific character string, possibly
  \code{NA} or an empty string (which on some OSes means \samp{UTC})..
  For the default \code{location = TRUE} this will be a location such as
  \code{"Europe/London"} if one can be ascertained.  For \code{location
  = FALSE} this may be an abbreviation such as \code{"EST"} or
  \code{"CEST"} on Windows.

  \code{OlsonNames} returns a character vector.
}

\section{Time zone names}{
  Where OSes describe their valid time zones can be obscure.  The help
  for the C function \code{tzset} can be helpful, but it can also be
  inaccurate.  There is a cumbersome POSIX specification (listed under
  environment variable \env{TZ} at
  \url{http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08}),
  which is often at least partially supported, but there are other more
  user-friendly ways to specify time zones.

  Almost all \R platforms make use of a time-zone database originally
  compiled by Arthur David Olson and now managed by IANA, in which the
  preferred way to refer to a time zone is by a location (typically of a
  city) e.g. \code{Europe/London}, \code{America/Los_Angeles},
  \code{Pacific/Easter}.  Some traditional designations are also allowed
  such as \code{EST5EDT} or \code{GB}.  (Beware that some of these
  designations may not be what you expect: in particular \code{EST} is a
  time zone used in Canada \emph{without} daylight saving time, and not
  \code{EST5EDT} nor (Australian) Eastern Standard Time.)  The
  designation can also be an optional colon prepended to the path to a
  file giving complied zone information (and the examples above are all
  files in a system-specific location).  See
  \url{http://www.twinsun.com/tz/tz-link.htm} for more details and
  references.  By convention, regions with a unique time-zone history
  since 1970 have specific names in the database, but those with
  different earlier histories may not.  Each time zone has one or two
  (the second for DST) \emph{abbreviations} used when formatting times.

  The abbreviations used have changed over the years: for example France
  used \samp{PMT} (\sQuote{Paris Mean Time}) from 1891 to 1911 then
  \samp{WET/WEST} up to 1940 and \samp{CET/CEST} from 1946.  (In almost
  all time zones they have been stable since 1970.)  The POSIX standard
  allows only one or two abbreviations per time zone, so you may see the
  current abbreviation(s) used for older times: more so in versions of
  \R prior to 3.1.0.
  
  The function \code{OlsonNames} returns the time-zone names known to
  the Olson/IANA database on the current system. The system-specific
  location in the file system varies, e.g.  \file{/usr/share/zoneinfo}
  (Linux, OS X, FreeBSD), \file{/usr/share/lib/zoneinfo} (Solaris, AIX),
  \ldots.  It is likely that there is a file named something like
  \file{zone.tab} under that directory listing the locations known as
  time-zone names (but not for example \code{EST5EDT}): this is read by
  \code{OlsonNames}.  See also
  \url{http://en.wikipedia.org/wiki/Zone.tab}.
#ifdef unix

  Where \R was configured with option \option{--with-internal-tzcode}
  (the default on OS X and Windows: recommended on Solaris), the
  database at \code{file.path(R.home("share"), "zoneinfo")} is used by
  default: file \file{VERSION} in that directory states the version.
  Environment variable \env{TZDIR} can be used to point to a different
  \file{zoneinfo} directory (this is also supported by some OSes,
  e.g. Linux).
#endif
#ifdef windows

  Under Windows the current version of the database is stated in
  file \file{\var{\link{R_HOME}}\\share\\zoneinfo\\VERSION}.
  Environment variable \env{TZDIR} can be used to point to a different
  \file{zoneinfo} directory.

  An attempt is made (once only per session) to map Windows' idea of the
  current time zone to a location, following an earlier version of
  \url{http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml}
  It can be overridden by setting the \env{TZ} environment variable
  before any date-times are used in the session.

  Windows documents a specification of the form \code{GST-1GDT}: this is
  interpreted as POSIX-like and hence the \sQuote{US rules} for changing
  to/from DST are applied (and are incorrect for Germany).  This is
  accepted for backwards compatibility but is unreliable and gives a
  warning.
#endif

  Most platforms support time zones of the form \samp{GMT+n} and
  \samp{GMT-n}, which assume at a fixed offset from UTC (hence no DST).
  Contrary to some expectations (but consistent with names such as
  \samp{PST8PDT}), negative offsets are times ahead of (east of) UTC,
  positive offsets are times behind (west of) UTC.
}

\note{
  Since 2007 there has been considerable disruption over changes to the
  timings of the DST transitions, aimed at energy conservation.  These
  often have short notice and time-zone databases may not be up to date.
  (Chile has in the recent past announced changes at \emph{5 days}
  notice, and in 2014 Turkey gave only a month's notice.)

  Note that except where replaced, the operation of time zones is an OS
  service, and even where replaced a third-party database is used and
  can be updated (see the section on \sQuote{Time zone names}).
  Incorrect results will never be an \R issue, so please ensure that you
  have the courtesy not to blame \R for them.
}
% Unlike Tony Plate.

\seealso{
  \code{\link{Sys.time}}, \code{\link{as.POSIXlt}}.

  \url{http://en.wikipedia.org/wiki/Time_zone} and
  \url{http://www.twinsun.com/tz/tz-link.htm} for extensive sets of links.
}

\examples{
Sys.timezone()

str(OlsonNames()) ## a few hundred names
}
\keyword{utilities}
\keyword{chron}
