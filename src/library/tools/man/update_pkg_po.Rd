% File src/library/tools/man/update_pkg_po.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2013 R Core Team
% Distributed under GPL 2 or later

\name{update_pkg_po}
\alias{update_pkg_po}
\title{
  Prepare Translations for a Package
}
\description{
  Prepare the \file{po} directory of a package and compile and install
  the translations.
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
    template files.}
}
\details{
  This performs a series of steps to prepare or update messages in
  the package.
  \itemize{
    \item If the package sources do not already have a \file{po}
    directory, one is created.
    \item \code{\link{xgettext2pot}} is called to create/update a file
    \file{po/R-\var{pkgname}.pot} containing the translatable messages
    in the package.
    \item All existing files in directory \code{po} with names
    \file{R-\var{lang}.po} are updated from \file{R-\var{pkgname}.pot},
    \code{\link{checkPoFile}} is called on the updated file, and if
    there are no problems the file is  compiled and installed under
    \file{inst/po}.
    \item In a UTF-8 locale, a \sQuote{translation} \file{R-en@quot.po}
    is created with UTF-8 directional quotes, compiled and installed
    under \file{inst/po}.
   \item The remaining steps are done only if file
    \file{po/\var{pkgname}.pot} already exists.  The
    \file{src/*.\{c,cc,cpp,m,mm\}} files in the package are examined to
    create a file \file{po/\var{pkgname}.pot} containing the translatable
    messages in the C/C++ files.  If there is a \code{src/windows}
    directory, files within it are also examined.
    \item All existing files in directory \code{po} with names
    \file{\var{lang}.po} are updated from \file{\var{pkgname}.pot},
    \code{\link{checkPoFile}} is called on the updated file, and if
    there are no problems the file is  compiled and
    installed under \file{inst/po}.
    \item In a UTF-8 locale, a \sQuote{translation} \file{en@quot.po} is
    created with UTF-8 directional quotes, compiled and installed under
    \file{inst/po}.
  }

  Note that C/C++ messages are not automatically prepared for
  translation as they need to be explicitly marked for translation in
  the source files.  Once that has been done, create an empty file
  \file{po/\var{pkgname}.pot} in the package sources and run this
  function again.

  \code{pkg = "base"} is special (and for use by \R developers only):
  the C files are not in the package directory but in the main sources.
}

\section{System requirements}{
  This function requires the following tools from the GNU
  \command{gettext-tools}: \command{xgettext}, \command{msgmerge},
  \command{msgfmt}, \command{msginit} and \command{msgconv}.  These are
  part of most Linux distributions and easily compiled from the sources
  on Unix-alikes (including OS X).  Pre-compiled versions for
  Windows are available in
  \url{http://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip}.

  It will probably not work correctly for \code{en@quot} translations
  except in a UTF-8 locale, so these are skipped elsewhere.
}

\seealso{
  \code{\link{xgettext2pot}}.
}
\keyword{ utilities }
