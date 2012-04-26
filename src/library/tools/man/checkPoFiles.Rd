% File src/library/tools/man/checkRdaFiles.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012 R Core Team
% Distributed under GPL 2 or later

\name{checkPoFiles}
\alias{checkPoFile}
\alias{checkPoFiles}
\title{
  Check translation files for inconsistent format strings.
}
\description{
  These functions compare formats embedded in English messages
  with translated strings to check for consistency.  \code{checkPoFile}
  checks one file, while \code{checkPoFiles} checks all files for a specified
  language.
}
\usage{
checkPoFile(f, strictPlural = FALSE)
checkPoFiles(language, dir)
}
\arguments{
  \item{f}{a character string giving a single filepath.}
  \item{strictPlural}{whether to compare formats of singular and plural
    forms in a strict way.}
  \item{language}{a character string giving a language code.}
  \item{dir}{a path to a directory in which to check files.}
}
\details{
  
  Part of \R's internationalization depends on translations of messages
  in \file{.po} files.  In these files an \sQuote{English} message taken
  from the \R sources is followed by a translation into another
  language.  Many of these messages are format strings for C or \R
  \code{\link{sprintf}} and related functions.  In these cases, the
  translation must give a compatible format or an error will be
  generated when the message is displayed.

  The rules for compatibility differ between C and \R in several ways. C
  supports several conversions not supported by \R, namely \code{c, u,
  p, n}.  It is allowed in C's \code{sprintf()} function to have more
  arguments than are needed by the format string, but in \R the counts
  must match exactly.  \R requires types of arguments to match, whereas
  C will do the display whether it makes sense or not.

  These functions compromise on the testing as follows.  The additional
  formats allowed in C are accepted, and all differences in argument
  type or count are reported.  As a consequence some reported
  differences are not errors.

  If the \code{strictPlural} argument is \code{TRUE}, then argument
  lists must agree exactly between singular and plural forms of messages; if
  \code{FALSE}, then translations only need to match one or the other
  of the two forms.  When \code{checkPoFiles} calls \code{checkPoFile}, 
  the \code{strictPlural} argument is set to \code{TRUE} for files with
  names starting \file{R-}, and to \code{FALSE} otherwise.
  
  Items marked as \sQuote{fuzzy} in the \file{.po} file are not
  processed (as they are ignored by the message compiler).
}
\note{
  The code will currently report differences where the \samp{\%2$d}
  notation is used to re-order the formats: such translations need to be
  checked manually.
}
\value{
  Both functions return an object of S3 class \code{"check_po_files"}.  A
  \code{print} method is defined for this class to display a report on
  the differences.
}
\references{
  See the GNU gettext manual for the \file{.po} file format:\cr
  \url{http://www.gnu.org/software/gettext/manual/gettext.html}.
}
\author{
  Duncan Murdoch
}
\seealso{
  \code{\link{xgettext}}, \code{\link{sprintf}}
}
\examples{
\dontrun{
checkPoFiles("de", "/path/to/R/src/directory")
}
}
\keyword{ utilities }
