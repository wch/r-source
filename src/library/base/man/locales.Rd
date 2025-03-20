% File src/library/base/man/locales.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{locales}
\title{Query or Set Aspects of the Locale}

\alias{locales}
\alias{Sys.getlocale}
\alias{Sys.setlocale}
\alias{.LC.categories}
\alias{LC_ALL}
\alias{LC_COLLATE}
\alias{LC_CTYPE}
\alias{LC_MONETARY}
\alias{LC_NUMERIC}
\alias{LC_TIME}
%% Next 3  were only 'ifdef unix':
\alias{LC_MESSAGES}
\alias{LC_PAPER}
\alias{LC_MEASUREMENT}

\description{
  Get details of or set aspects of the locale for the \R process.
}
\usage{
Sys.getlocale (category = "LC_ALL")
Sys.setlocale (category = "LC_ALL", locale = "")
.LC.categories
}
\arguments{
  \item{category}{character string.  The following categories should
    always be supported: \code{"LC_ALL"}, \code{"LC_COLLATE"},
    \code{"LC_CTYPE"}, \code{"LC_MONETARY"}, \code{"LC_NUMERIC"} and
    \code{"LC_TIME"}.  Some systems (not Windows) will also support
    \code{"LC_MESSAGES"}, \code{"LC_PAPER"} and \code{"LC_MEASUREMENT"}.
    These category names are available in \code{.LC.categories}; even when
    not supported, \code{Sys.getlocale(.)} will return \code{""}, e.g., for
    the \code{"LC_PAPER"} example on Windows.
  }
  \item{locale}{character string.  A valid locale name on the system in
    use.  Normally \code{""} (the default) will pick up the default
    locale for the system.}
}
\details{
  The locale describes aspects of the internationalization of a program.
  Initially most aspects of the locale of \R are set to \code{"C"}
  (which is the default for the C language and reflects North-American
  usage -- also known as \code{"POSIX"}).  \R sets \code{"LC_CTYPE"} and
  \code{"LC_COLLATE"}, which allow the use of a different character set
  and alphabetic comparisons in that character set (including the use of
  \code{\link{sort}}), \code{"LC_MONETARY"} (for use by
  \code{\link{Sys.localeconv}}) and \code{"LC_TIME"} may affect the
  behaviour of \code{\link{as.POSIXlt}} and \code{\link{strptime}} and
  functions which use them (but not \code{\link{date}}).

  The first seven categories described here are those specified by
  POSIX.  \code{"LC_MESSAGES"} will be \code{"C"} on systems that do not
  support message translation, and is not supported on Windows, where
  you \emph{must} use the \env{LANGUAGE} environment variable for
  message translation, see below and the \code{\link{Sys.setLanguage}()}
  utility.  Trying to use an unsupported category is an error for
  \code{Sys.setlocale}.

  Note that setting category \code{"LC_ALL"} sets only categories
  \code{"LC_COLLATE"}, \code{"LC_CTYPE"}, \code{"LC_MONETARY"} and
  \code{"LC_TIME"}.

  Attempts to set an invalid locale are ignored.  There may or may not
  be a warning, depending on the OS.

  Attempts to change the character set (by
  \code{Sys.setlocale("LC_CTYPE", )}, if that implies a different
  character set) during a session may not work and are likely to lead to
  some confusion.

  Note that the \env{LANGUAGE} environment variable has precedence over
  \code{"LC_MESSAGES"} in selecting the language for message translation
  on most \R platforms.

  On platforms where ICU is used for collation the locale used for
  collation can be reset by \code{\link{icuSetCollate}}.  Except on
  Windows, the initial setting is taken from the \code{"LC_COLLATE"}
  category, and it is reset when this is changed by a call to
  \code{Sys.setlocale}.
}
\value{
  A character string of length one describing the locale in use (after
  setting for \code{Sys.setlocale}), or an empty character string if the
  current locale settings are invalid or \code{NULL} if locale
  information is unavailable.

  For \code{category = "LC_ALL"} the details of the string are
  system-specific: it might be a single locale name or a set of locale
  names separated by \code{"/"} (macOS) or \code{";"}
  (Windows, Linux).  For portability, it is best to query categories
  individually: it is not necessarily the case that the result of
  \code{foo <- Sys.getlocale()} can be used in
  \code{Sys.setlocale("LC_ALL", locale = foo)}.
}

\section{Available locales}{
  On most Unix-alikes the POSIX shell command \command{locale -a} will
  list the \sQuote{available public} locales.  What that means is
  platform-dependent.  On recent Linuxen this may mean \sQuote{available
  to be installed} as on some RPM-based systems the locale data is in
  separate \abbr{RPM}s.  On Debian/Ubuntu the set of available locales is
  managed by OS-specific facilities such as \command{locale-gen} and
  \command{locale -a} lists those currently enabled.

  For Windows, Microsoft moves its documentation frequently so a Web
  search is the best way to find current information.  From \R 4.2, \abbr{UCRT}
  locale names should be used.  The character set should match the
  system/ANSI codepage (\code{l10n_info()$codepage} be the same as
  \code{l10n_info()$system.codepage}).  Setting it to any other value
  results in a warning and may cause encoding problems.  As from \R 4.2
  on recent Windows the system codepage is 65001 and one should always
  use locale names ending with \code{".UTF-8"} (except for \code{"C"}
  and \code{""}), otherwise Windows may add a different character set.
}

% assume this just affects strtod/atof, scanf/printf and friends,
% as seems to be the case in glibc.
\section{Warning}{
  Setting \code{"LC_NUMERIC"} to any value other than \code{"C"} may
  cause \R to function anomalously, so gives a warning.  Input
  conversions in \R itself are unaffected, but the reading and writing
  of ASCII \code{\link{save}} files will be, as may packages which do
  their own input/output.

  Setting it temporarily on a Unix-alike to produce graphical or text
  output may work well enough, but \code{\link{options}(OutDec)} is
  often preferable.

  Almost all the output routines used by \R itself under Windows ignore
  the setting of \code{"LC_NUMERIC"} since they make use of the Trio
  library which is not internationalized.
}

\note{
  Changing the values of locale categories whilst \R is running ought
  to be noticed by the OS services, and usually is but exceptions have
  been seen (usually in collation services).

  Do not use the value of \code{Sys.getlocale("LC_CTYPE")} to attempt to
  find the character set -- for example UTF-8 locales can have suffix
  \samp{.UTF-8} or \samp{.utf8} (more common on Linux than \samp{UTF-8})
  or none (as on macOS) and Latin-9 locales can have suffix
  \samp{ISO8859-15}, \samp{iso885915}, \samp{iso885915@euro} or
  \samp{ISO8859-15@euro}.  Use \code{\link{l10n_info}} instead.
}

\seealso{
  \code{\link{strptime}} for uses of \code{category = "LC_TIME"}.
  \code{\link{Sys.localeconv}} for details of numerical and monetary
  representations.

  \code{\link{l10n_info}} gives some summary facts about the locale and
  its encoding (including if it is UTF-8).

  The \sQuote{R Installation and Administration} manual for background
  on locales and how to find out locale names on your system.

  \code{\link{Sys.setLanguage}}
}
\examples{
Sys.getlocale()

## Date-time  related :
Sys.getlocale("LC_TIME") -> olcT
then <- as.POSIXlt("2001-01-01 01:01:01", tz = "UTC")
\dontrun{
c(m = months(then), wd = weekdays(then)) # locale specific
Sys.setlocale("LC_TIME", "de")     # Solaris: details are OS-dependent
Sys.setlocale("LC_TIME", "de_DE")  # Many Unix-alikes
Sys.setlocale("LC_TIME", "de_DE.UTF-8")  # Linux, macOS, other Unix-alikes
Sys.setlocale("LC_TIME", "de_DE.utf8")   # some Linux versions
Sys.setlocale("LC_TIME", "German.UTF-8") # Windows
Sys.getlocale("LC_TIME") # the last one successfully set above
c(m = months(then), wd = weekdays(then)) # in C_TIME locale 'cT' ; typically German
}
Sys.setlocale("LC_TIME", "C")
c(m = months(then), wd = weekdays(then)) # "standard" (still platform specific ?)
Sys.setlocale("LC_TIME", olcT)           # reset to previous

## Other locales
Sys.getlocale("LC_PAPER")          # may or may not be set
.LC.categories # of length 9 on all platforms

\dontrun{Sys.setlocale("LC_COLLATE", "C")   # turn off locale-specific sorting,
                                   # usually (but not on all platforms)
Sys.setenv("LANGUAGE" = "es") # set the language for error/warning messages
}
\donttest{## some nice formatting; should work on most platforms,
          ## macOS does not name the entries.
 sep <- switch(Sys.info()[["sysname"]],
               "Darwin"=, "SunOS" = "/",
               "Linux" =, "Windows" = ";")
 ##' named vector from a "full" Sys.getlocale() :
 asNvec <- function(loc) {
     sl <- strsplit(strsplit(loc, sep)[[1L]], "=")
     if(all(lengths(sl) == 2L))
        setNames(sapply(sl, `[[`, 2L), sapply(sl, `[[`, 1L))
     else
       setNames(as.character(sl), .LC.categories[1+seq_along(sl)])
 }
 print.Dlist(lloc <- asNvec(Sys.getlocale()))
 ## R-supported ones (but LC_ALL):
 lloc[.LC.categories[-1]]
}
}
\keyword{utilities}
