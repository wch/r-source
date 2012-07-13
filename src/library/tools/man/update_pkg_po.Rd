% File src/library/tools/man/xgettext.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012 R Core Team
% Distributed under GPL 2 or later

\name{update_pkg_po}
\alias{update_pkg_po}
\title{
  Prepare Translations for a Package
}
\description{
  Prepare the \file{po} directory of a packages.
}
\usage{
update_pkg_po(pkgdir, pkg = NULL, version = NULL, copyright, bugs)
}
\arguments{
  \item{pkgdir}{The path to the package directory.}
  \item{pkg}{The package name: if \code{NULL} it is read from the package's
    \file{DESCRIPTION} file.}
  \item{version}{The package version: if \code{NULL} it is read from the
    package's \file{DESCRIPTION} file.}
  \item{copyright, bugs}{optional character strings for the
    \samp{Copyright} and \samp{Report-Msgid-Bugs-To} details in the
    template file.}
}
\details{
  This performs a series of steps to prepare for or update messages in
  the package.
  \itemize{
    \item If the package sources do not already have a \file{po}
    directory, one is created.
    \item \code{\link{xgettext2pot}} is called to create/update a file
    \file{po/R-\var{pkgname}.pot} containing the translatable messages
    in the package.
    \item All existing files in directory \code{po} with names
    \file{R-\var{lang}.po} are updated from \file{R-\var{pkgname}.pot}.
    \item \code{\link{checkPoFile}} is called on each
    \file{po/R-\var{lang}.po} file.
    \item A file \file{po/R-en@quot.po} is created with UTF-8 directional
    quotes.
    \item All the \file{po/R-\var{lang}.po} files are compiled and
    installed under \file{inst/po}.
    \item The remaining steps are done only if file
    \file{po/\var{pkgname}.pot} already exists.  All the \code{src/*.c}
    files in the package are examined to create a file
    \file{po/\var{pkgname}.po} containing the translatable messages
    in the C files.
    \item All existing files in directory \code{po} with names
    \file{\var{lang}.po} are updated from \file{\var{pkgname}.pot}.
    \item \code{\link{checkPoFile}} is called on each
    \file{po/\var{lang}.po} file.
    \item A file \file{po/en@quot.po} is created with UTF-8 directional
    quotes.
    \item All the \file{po/\var{lang}.po} files are compiled and
    installed under \file{inst/po}.
  }
}
\section{System requirements}{
  This function requires the following tools from the GNU
  \command{gettext-tools}: \command{xgettext}, \command{msgmerge},
  \command{msgfmt}, \command{msginit} and \command{msgconv}.  These are
  part of most Linux distributions and easily compiled from the sources
  on Unix-alikes (including Mac OS X).  Pre-compiled versions for
  (32- or 64-bit) Windows are available from
  \url{http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip}.
}
\seealso{
  \code{\link{xgettext2pot}}.
}
\keyword{ utilities }
