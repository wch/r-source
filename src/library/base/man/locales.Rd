\name{locales}
\alias{locales}
\alias{Sys.getlocale}
\alias{Sys.setlocale}
\title{Query or Set Aspects of the Locale}
\description{
  Get details of or set aspects of the locale for the \R process.
}
\usage{
Sys.getlocale(category = "LC_ALL")
Sys.setlocale(category = "LC_ALL", locale = "")
}
\arguments{
  \item{category}{character string. Must be one of \code{"LC_ALL"},
    \code{"LC_COLLATE"}, \code{"LC_CTYPE"}, \code{"LC_MONETARY"},
    \code{"LC_NUMERIC"} or \code{"LC_TIME"}.}
  \item{locale}{character string.  A valid locale name on the system in
    use. Normally \code{""} (the default) will pick up the default
    locale for the system.}
}
\details{
  The locale describes aspects of the internationalization of a program.
  Initially most aspects of the locale of \R are set to \code{"C"} (which
  is the default for the C language and reflects North-American usage).
  \R sets \code{"LC_CTYPE"} and \code{"LC_COLLATE"}, which allow the
  use of a different character set and alphabetic
  comparisons in that character set (including the use of
  \code{\link{sort}}), \code{"LC_MONETARY"} (for use by
  \code{\link{Sys.localeconv}}) and \code{"LC_TIME"} may affect
  the behaviour of \code{\link{as.POSIXlt}} and \code{\link{strptime}}
  and functions which use them (but not \code{\link{date}}).

  \R can be built with no support for locales, but it is normally
  available on Unix and is available on Windows.

  Some systems will have other locale categories, but the six described
  here are those specified by POSIX.

  Note that setting \code{"LC_ALL"} as from \R 2.1.0 sets only
  \code{"LC_COLLATE"}, \code{"LC_CTYPE"}, \code{"LC_MONETARY"} and
  \code{"LC_TIME"}.
}
\value{
  A character string of length one describing the locale in use (after
  setting for \code{Sys.setlocale}), or an empty character string if the
  locale is invalid (with a warning) or \code{NULL} if locale
  information is unavailable.

  For \code{category = "LC_ALL"} the details of the string are
  system-specific: it might be a single locale or a set of locales
  separated by \code{"/"} (Solaris) or \code{";"} (Windows).  For
  portability, it is best to query categories individually.  It is
  guaranteed that the result of \code{foo <- Sys.getlocale()} can be
  used in \code{Sys.setlocale("LC_ALL", locale = foo)} on the same
  machine.
}
\section{Warning}{
  Setting \code{"LC_NUMERIC"} may cause \R to function
  anomalously, so gives a warning.  (The known problems are with
  input conversion in locales with \code{,} as the decimal point.)
  Setting it temporarily to produce graphical or text output may work
  well enough.
}
\seealso{
  \code{\link{strptime}} for uses of \code{category = "LC_TIME"}.
  \code{\link{Sys.localeconv}} for details of numerical and monetary
  representations.
}
\examples{
Sys.getlocale()
Sys.getlocale("LC_TIME")
\dontrun{
Sys.setlocale("LC_TIME", "de")     # Solaris: details are OS-dependent
Sys.setlocale("LC_TIME", "German") # Windows}

Sys.setlocale("LC_COLLATE", "C")  # turn off locale-specific sorting
}
\keyword{utilities}
