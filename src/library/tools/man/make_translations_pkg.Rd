% File src/library/tools/man/make_translations_pkg.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{make_translations_pkg}
\alias{make_translations_pkg}
\title{
  Package the Current Translations in the \R sources
}
\description{
  A utility for R Core members to prepare a package of updated translations.
}
\usage{
make_translations_pkg(srcdir, outDir = ".", append = "-1")
}
\arguments{
  \item{srcdir}{The \R source directory.}
  \item{outDir}{The directory into which to place the prepared package.}
  \item{append}{The suffix for the package version number,
    e.g.\sspace{}\code{3.0.0-1} will be the default in \R 3.0.0.}
}
\details{
  This extracts the translations in a current \R source distribution and
  packages them as a source package called \pkg{translations} which can
  be distributed on CRAN and installed by
  \code{\link{update.packages}}.  This allows e.g.\sspace{}the translations
  shipped in \R 3.x.y to be updated to those currently in
  \samp{R-patched}, even by a user without administrative privileges.

  The package has a \samp{Depends} field which restricts it to versions
  \code{3.x.*} for a single \code{x}.
}

\keyword{utilities}
