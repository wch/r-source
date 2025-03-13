% File src/library/tools/man/xgettext.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{xgettext}
\title{Extract Translatable Messages from R Files in a Package}
\alias{xgettext}
\alias{xngettext}
\alias{xgettext2pot}
\description{
  For each file in the \file{R} directory (including system-specific
  subdirectories) of a \emph{source} package, extract the unique arguments passed
  to these \dQuote{message generating} calls;
  \describe{
    \item{for \code{xgettext()}:}{% see c("warning", "stop", ..) in ../R/xgettext.R :
      to \code{\link{stop}}, \code{\link{warning}}, \code{\link{message}},
      \code{\link{packageStartupMessage}}, \code{\link{gettext}} and
      \code{\link{gettextf}},}

    \item{for \code{xngettext()}:}{
      to \code{\link{ngettext}}.}
  }

  \code{xgettext2pot()} calls both \code{xgettext()} and then \code{xngettext()}.
}
\usage{
xgettext(dir, verbose = FALSE, asCall = TRUE)

xngettext(dir, verbose = FALSE)

xgettext2pot(dir, potFile, name = "R", version, bugs)
}
\arguments{
  \item{dir}{the directory of a source package, i.e., with a \file{./R} sub
    directory.}
  \item{verbose}{logical: should each file be listed as it is processed?}
  \item{asCall}{logical: if \code{TRUE} each argument is converted to
    string and returned whole, otherwise the string literals within each
    argument are extracted (recursively). See Examples.}

  \item{potFile}{name of \code{po} template file to be produced.
    Defaults to \file{R-\var{pkgname}.pot} where
    \var{pkgname} is the basename of \file{dir}.}
  \item{name, version, bugs}{as recorded in the template file:
    \code{version} defaults the version number of the currently running
    \R, and \code{bugs} to \code{"bugs.r-project.org"}.}
}
\details{
  Leading and trailing white space (space, tab and linefeed
  (aka newline, i.e., \samp{\\n})) is removed
  for all the calls extracted by \code{xgettext()}, see
  \sQuote{Description} above,
  as it is by the internal code that passes strings for translation.

  We look to see if the matched functions were called with
  \code{domain = NA}.  If so, when \code{asCall} is true, the whole call
  is omitted.  Note that a call might contain a nested call
  to \code{gettext} (or \code{warning}, etc.) whose strings would be visible
  if \code{asCall} is false.

  \code{xgettext2pot} calls \code{xgettext} and then \code{xngettext},
  and writes a PO template file (to \code{potFile}) for use with the GNU \I{gettext}
  tools.  This ensures that the strings for simple translation are
  unique in the file (as GNU \I{gettext} requires), but does not do so
  for \code{ngettext} calls (and the rules are not stated in the GNU \I{gettext}
  manual, but \command{msgfmt} complains if there is duplication between
  the sets.).

  If applied to the \pkg{base} package, this also looks in the \file{.R}
  files in \file{\var{\link{R_HOME}}/share/R}.
}
\value{
  For \code{xgettext}, a list of objects of class \code{"xgettext"}
  (which has a \code{print} method), one per source file that
  contains potentially translatable strings.

  For \code{xngettext}, a list of objects of class \code{"xngettext"},
  which are themselves lists of length-2 character vectors.
}
\seealso{
  \code{\link{update_pkg_po}()} which calls \code{xgettext2pot()}.
}
\examples{\dontrun{## in a source-directory build (not typical!) of R;
## otherwise, download and unpack the R sources, and replace
## R.home()  by  "<my_path_to_source_R>" :
xgettext(file.path(R.home(), "src", "library", "splines"))
}% dont..

## Create source package-like  <tmp>/R/foo.R  and get text from it:
tmpPkg <- tempdir()
tmpRDir <- file.path(tmpPkg, "R")
dir.create(tmpRDir, showWarnings = FALSE)
fnChar <- paste(sep = "\n",
  "foo <- function(x) {",
  "  if (x < -1)  stop('too small')",
  "  # messages unduplicated (not so for ngettext)",
  "  if (x < -.5) stop('too small')",
  "  if (x < 0) {",
  "    warning(",
  "      'sqrt(x) is', sqrt(as.complex(x)),",
  "      ', which may be too small'",
  "    )",
  "  }",
  "  # calls with domain=NA are skipped",
  "  if (x == 0) cat(gettext('x is 0!\n', domain=NA))",
  "  # gettext strings may be ignored due to 'outer' domain=NA",
  "  if (x > 10) warning('x is ', gettextf('\%.2f', x), domain=NA)",
  "  # using a custom condition class",
  "  if (x == 42)",
  "    stop(errorCondition(gettext('needs Deep Thought'), class='myError'))",
  "  x",
  "}")

writeLines(fnChar, con = file.path(tmpRDir, "foo.R"))

## [[1]] : suppressing (tmpfile) name to make example Rdiff-able
xgettext(tmpPkg, asCall=TRUE )[[1]] # default; shows calls
xgettext(tmpPkg, asCall=FALSE)[[1]] # doesn't ; but then ' \%.2f '

unlink(tmpRDir, recursive=TRUE)
}
\keyword{ utilities }
