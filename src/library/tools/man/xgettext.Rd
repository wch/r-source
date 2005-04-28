\name{xgettext}
\alias{xgettext}
\alias{xngettext}
\alias{xgettext2pot}
\title{Extract Translatable Messages from R Files in a Package}
\description{
  For each file in the \file{R} directory (including system-specific
  subdirectories) of a package, extract the unique arguments passed
  to \code{\link{stop}}, \code{\link{warning}}, \code{\link{message}},
  \code{\link{gettext}} and \code{\link{gettextf}}, or to
  \code{\link{ngettext}}.
}
\usage{
xgettext(dir, verbose = FALSE, asCall = TRUE)

xngettext(dir, verbose = FALSE)

xgettext2pot(dir, potFile)
}
\arguments{
  \item{dir}{the directory of a source package.}
  \item{verbose}{logical: should each file be listed as it is processed?}
  \item{asCall}{logical: if \code{TRUE} each argument is returned whole,
    otherwise the strings within each argument are extracted.}

  \item{potFile}{name of \code{po} template file to be
    produced. Defaults to \code{"R-pkg.pot"} where \code{pkg} is the
    basename of \code{dir}.}
}
\details{
  Leading and trailing white space (space, tab and linefeed) is removed
  for calls to \code{gettext}, \code{gettextf}, \code{stop},
  \code{warning}, and \code{message}, as it is by the internal code that
  passes strings for translation.

  We look to see if these functions were called with \code{domain = NA}
  and if so omit the call if \code{asCall = TRUE}: note that the
  call might contain a call to \code{gettext} which would be visible if
  \code{asCall = FALSE}.

  \code{xgettext2pot} calls \code{xgettext} and then \code{xngettext},
  and writes a PO template file for use with the \pkg{GNU Gettext}
  tools.  This ensures that the strings for simple translation are
  unique in the file (as \pkg{GNU Gettext} requires), but does not do so
  for \code{ngettext} calls (and the rules are not stated in the Gettext
  manual).

  If applied to the \code{base} package, this also looks in the \code{.R}
  files in \file{R_HOME/share/R}.
}
\value{
  For \code{xgettext}, a list of objects of class \code{"xgettext"}
  (which has a \code{print} method), one per source file that
  potentially contains translatable strings

  For \code{nxgettext}, a list of objects of class \code{"xngettext"},
  which are themselves lists of length-2 character strings.
}
\examples{\dontrun{## in a source-directory build of R:
xgettext(file.path(R.home(), "src", "library", "splines"))
}}
\keyword{ utilities }
