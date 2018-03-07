% File src/library/base/man/timezones.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
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
\alias{.sys.timezone}

\title{Time Zones}
\description{
  Information about time zones in \R.  \code{Sys.timezone} returns
  the name of the current time zone.
}

\usage{
Sys.timezone(location = TRUE)

OlsonNames(tzdir = NULL)
}

\arguments{
  \item{location}{logical: defunct: ignored, with a warning for false values.}
  
  \item{tzdir}{The time-zone database to be used: the default is to try
    known locations until one is found.}
}

\details{
  Time zones are a system-specific topic, but these days almost all \R
  platforms use similar underlying code, used by Linux, macOS, Solaris,
  AIX and FreeBSD, and installed with \R on Windows.  (Unfortunately
  there are many system-specific errors in the implementations.)  It is
  possible to use the \R sources' version of the code on Unix-alikes as
  well as on Windows: this is the default for macOS and recommended for
  Solaris.

  It should be possible to set the current time zone via the environment
  variable \env{TZ}: see the section on \sQuote{Time zone names} for
  suitable values.  \code{Sys.timezone()} will return the value of
  \env{TZ} if set initially (and on some OSes it is always set),
  otherwise it will try to retrieve from the OS a value which if set for
  \env{TZ} would give the initial time zone. (\sQuote{Initially} means
  before any time-zone functions are used: if \env{TZ} is being set to
  override the OS setting or if the \sQuote{try} does not get this
  right, it should be set before the \R process is started or (probably
  early enough) in file \code{\link{.Rprofile}}).
  
  %% glibc silently uses UTC but uses the invalid name as the
  %% abbreviations.  tzcode as used by R warns and uses UTC.
  If \env{TZ} is set but invalid, most platforms default to \samp{UTC},
  the time zone colloquially known as \samp{GMT} (see
  \url{https://en.wikipedia.org/wiki/Coordinated_Universal_Time}).
  (Some but not all platforms will give a warning for invalid values.)
  If it is unset or empty the \emph{system} time zone is used (the one
  returned by \code{Sys.timezone}).

  Time zones did not come into use until the second half of the
  nineteenth century and were not widely adopted until the twentieth,
  and \emph{daylight saving time} (DST, also known as \emph{summer
  time}) was first introduced in the early twentieth century, most
  widely in 1916.  Over the last 100 years places have changed their
  affiliation between major time zones, have opted out of (or in to) DST
  in various years or adopted DST rule changes late or not at all.  (The
  UK experimented with DST throughout 1971, only.)

  A quite common system implementation of \code{POSIXct} is as signed
  32-bit integers and so only goes back to the end of 1901: on such
  systems \R assumes that dates prior to that are in the same time zone
  as they were in 1902.  Most of the world had not adopted time zones by
  1902 (so used local \sQuote{mean time} based on longitude) but for a
  few places there had been time-zone changes before then.  64-bit
  representations are becoming common; unfortunately on some 64-bit OSes
  (notably macOS) the database information is 32-bit and so only
  available for the range 1901--2038, and incompletely for the end
  years.

  As from \R 3.5.0, when a time zone location is first found in a
  session, its value is cached in object \code{.sys.timezone} in the
  base environment.
}

\value{
  \code{Sys.timezone} returns an OS-specific character string, possibly
  \code{NA} or an empty string (which on some OSes means \samp{UTC}).
  This will be a location such as \code{"Europe/London"} if one can be
  ascertained.

  A time zone region may be known by several names: for example
  \samp{"Europe/London"} is also known as \samp{GB}, \samp{GB-Eire},
  \samp{Europe/Belfast}, \samp{Europe/Guernsey},
  \samp{Europe/Isle_of_Man} and \samp{Europe/Jersey}.  A few regions
  are also known by a summary of their time zone,
  e.g.\sspace{}\samp{PST8PDT} is an alias for
  \samp{America/Los_Angeles}.

  \code{OlsonNames} returns a character vector, see the examples for
  typical cases.  It may have an attribute \code{"Version"}, something
  like \samp{"2017c"}.
}

\section{Time zone names}{
  Names \code{"UTC"} and its synonym \code{"GMT"} are accepted on all
  platforms.

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
  city), e.g., \code{Europe/London}, \code{America/Los_Angeles},
  \code{Pacific/Easter} within a \sQuote{time zone region}.  Some
  traditional designations are also allowed such as \code{EST5EDT} or
  \code{GB}.  (Beware that some of these designations may not be what
  you expect: in particular \code{EST} is a time zone used in Canada
  \emph{without} daylight saving time, and not \code{EST5EDT} nor
  (Australian) Eastern Standard Time.)  The designation can also be an
  optional colon prepended to the path to a file giving complied zone
  information (and the examples above are all files in a system-specific
  location).  See \url{http://www.twinsun.com/tz/tz-link.htm} for more
  details and references.  By convention, regions with a unique
  time-zone history since 1970 have specific names in the database, but
  those with different earlier histories may not.  Each time zone has
  one or two (the second for DST) \emph{abbreviations} used when
  formatting times.

  The abbreviations used have changed over the years: for example France
  used \samp{PMT} (\sQuote{Paris Mean Time}) from 1891 to 1911 then
  \samp{WET/WEST} up to 1940 and \samp{CET/CEST} from 1946.  (In almost
  all time zones the abbreviations have been stable since 1970.)  The
  POSIX standard allows only one or two abbreviations per time zone, so
  you may see the current abbreviation(s) used for older times.

  For some time zones abbreviations are like \samp{-03} and
  \samp{+0845}: this is done when there is no official abbreviation.
  (Negative values are behind (West of) UTC, as for the \code{"\%z"}
  format for \code{\link{strftime}}.)

  The function \code{OlsonNames} returns the time-zone names known to
  the currently selected Olson/IANA database.  The system-specific
  location in the file system varies,
  e.g.\sspace{}\file{/usr/share/zoneinfo} (Linux, macOS, FreeBSD),
  \file{/usr/share/lib/zoneinfo} (Solaris, AIX), \ldots.  It is likely
  that there is a file named something like \file{zone.tab} under that
  directory listing the locations known as time-zone names (but not for
  example \code{EST5EDT}).  See also
  \url{https://en.wikipedia.org/wiki/Zone.tab}.

  Where \R was configured with option \option{--with-internal-tzcode}
  (the default on macOS and Windows: recommended on Solaris), the database at
  \code{file.path(R.home("share"), "zoneinfo")} is used by default: file
  \file{VERSION} in that directory states the version.  Environment
  variable \env{TZDIR} can be used to point to a different
  \file{zoneinfo} database: this is also supported by the native
  services on some OSes, e.g.\sspace{}Linux using \code{glibc} except in
  secure modes.
  %% But not Linux with musl
  
  Time zones given by name (\emph{via} environment variable \env{TZ}, in
  \code{tz} arguments to functions such as \code{\link{as.POSIXlt}} and
  perhaps the system time zone) are loaded from the currently selected
  \file{zoneinfo} database.
#ifdef windows

  An attempt is made (once only per session) to map Windows' idea of the
  current time zone to a location, following a version of
  \url{http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml}
  with additional values deduced from the Windows Registry and documentation.
  It can be overridden by setting the \env{TZ} environment variable
  before any date-times are used in the session.
#endif

  Most platforms support time zones of the form \samp{Etc/GMT+n} and
  \samp{Etc/GMT-n} (possibly also without prefix \samp{Etc/}), which
  assume a fixed offset from UTC (hence no DST).  Contrary to some
  expectations (but consistent with names such as \samp{PST8PDT}),
  negative offsets are times ahead of (east of) UTC, positive offsets
  are times behind (west of) UTC.

  Immediately prior to the advent of legislated time zones, most people
  used time based on their longitude (or that of a nearby town), known
  as \sQuote{Local Mean Time} and abbreviated as \samp{LMT} in the
  databases: in many countries that was codified with a specific name
  before the switch to a standard time.  For example, Paris codified its
  LMT as \sQuote{Paris Mean Time} in 1891 (to be used throughout
  mainland France) and switched to \samp{GMT+0} in 1911.

  %% it is a ksh script so could well pop up elsewhere.
  Some systems (notably Linux) have a \command{tzselect} command which
  allows the interactive selection of a supported time zone name.  On
  systems using \command{systemd} (notably Linux), the OS command
  \command{timedatectl list-timezones} will list all available time zone
  names.
}

\section{Warning}{
%% glibc and macOS have _POSIX_TZNAME_MAX and define it as 6.
%% Earlier versions of R's code assumed 10, and it was discovered
%% that some implemntations did not abbreviate unusual names, thereby
%% exceeding this.
%% Olson's tzcode has a limit of 255 and does not check: this has been
%% corrected in R's copy.
%% sysconf(_SC_TZNAME_MAX) might allow it to be checked:
%% that gives 27 on macOS.  However, seems it is dynamic on glibc.
  There is a system-specific upper limit on the number of bytes in
  (abbreviated) time-zone names which can be as low as 6 (as required by
  POSIX).  Some OSes allow the setting of time zones with names which
  exceed their limit, and that can crash the \R session.
  
  \code{OlsonNames} tries to find an Olson database in known locations.
  It might not succeed (when it returns an empty vector with a warning)
  and even if it does it might not locate the database used by the
  date-time code linked into \R.  Fortunately names are added rarely
  and most databases are pretty complete.
}

\note{
  Since 2007 there has been considerable disruption over changes to the
  timings of the DST transitions, aimed at energy conservation.  These
  often have short notice and time-zone databases may not be up to date.
  (Morocco in 2013 announced a change to the end of DST at \emph{a days}
  notice, and in 2015 North Korea gave imprecise information about a
  change a week in advance.)

  On platforms with case-insensitive file systems, time zone names will be
  case-insensitive.  They may or may not be on other platforms and so,
  for example, \code{"gmt"} is valid on some platforms and not on others.

  Note that except where replaced, the operation of time zones is an OS
  service, and even where replaced a third-party database is used and
  can be updated (see the section on \sQuote{Time zone names}).
  Incorrect results will never be an \R issue, so please ensure that you
  have the courtesy not to blame \R for them.
}
% Unlike Tony Plate and John Pfuntner

#ifdef unix
\section{How the system time zone is found}{
  This section is of background interest for users of a Unix-alike, but
  may help if an \code{NA} value is returned unexpectedly.

  Commercial Unixen such as Solaris and AIX set \env{TZ}, so the value
  when \R is started is used.

  All other common platforms (Linux, macOS, *BSD) use similar schemes,
  either derived from \code{tzcode} (currently distributed from
  \url{https://www.iana.org/time-zones}) or independently coded
  (\code{glibc}, \code{musl-libc}).  Such systems read the time-zone
  information from a file \file{localtime}, usually under \file{/etc}
  (but possibly under \file{/usr/local/etc} or
  \file{/usr/local/etc/zoneinfo}).  As the usual Linux manual page for
  \code{localtime} says

  \sQuote{Because the time zone identifier is extracted from the symlink
    target name of \file{/etc/localtime}, this file may not be a normal
    file or hardlink.}
  
  Nevertheless, some Linux distributions (including the one from which
  that quote was taken) or sysadmins have chosen to copy a time-zone file
  to \file{localtime}.  For a non-symlink, the ultimate fallback is to
  compare that file to all files in the time-zone database.

  Some Linux platforms provide two other mechanisms which are tried in
  turn before looking at \file{/etc/localtime}.
  \itemize{
    \item \sQuote{Modern} Linux systems use \code{systemd} which
    provides mechanisms to set and retrieve the time zone (amongst other
    things).  There is a command \command{timedatectl} to give details.
    (Unfortunately RHEL/Centos 6.x are not \sQuote{modern}.)

    \item Debian-derived systems since \emph{ca} 2007 have supplied a
    file \file{/etc/timezone}.  Its format is undocumented but it
    empirically it contains a single line of text naming the time zone.
  }
  In each case a sanity check is performed that the time-zone name is the
  name of a file in the time-zone database.  (The systems probably use
  the time-zone file (symlinked to) \file{/etc/localtime}, but the
  \code{Sys.timezone} code
  does not check that is the same as the named file in the database.  This is deliberate
  as they may be from different dates.)
}
#endif

\seealso{
  \code{\link{Sys.time}}, \code{\link{as.POSIXlt}}.

  \url{https://en.wikipedia.org/wiki/Time_zone} and
  \url{http://www.twinsun.com/tz/tz-link.htm} for extensive sets of links.

  \url{https://data.iana.org/time-zones/theory.html} for the
  \sQuote{rules} of the Olson/IANA database.
}

\examples{
Sys.timezone()

str(OlsonNames()) ## typically close to 600 hundred names,
## typically some acronyms/aliases such as "UTC", "NZ", "MET", "Eire", ..., but
## mostly pairs (and triplets) such as "Pacific/Auckland"
table(sl <- grepl("/", OlsonNames()))
OlsonNames()[ !sl ] # the simple ones
head(Osl <- strsplit(OlsonNames()[sl], "/"))
(tOS1 <- table(vapply(Osl, `[[`, "", 1))) # Continents, countries, ...
table(lengths(Osl))# most are pairs, some triplets
str(Osl[lengths(Osl) >= 3])# "America" South and North ...
}
\keyword{utilities}
\keyword{chron}
