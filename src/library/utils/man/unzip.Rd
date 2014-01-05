% File src/library/utils/man/unzip.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{unzip}
\alias{unzip}
\title{
  Extract or List Zip Archives
}
\description{
  Extract files from or list a zip archive.
}
\usage{
unzip(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)
}
\arguments{
  \item{zipfile}{The pathname of the zip file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}

  \item{list}{If \code{TRUE}, list the files and extract none.  The
    equivalent of \command{unzip -l}.}

  \item{overwrite}{If \code{TRUE}, overwrite existing files, otherwise
    ignore such files.  The equivalent of \command{unzip -o}.}

  \item{junkpaths}{If \code{TRUE}, use only the basename of the stored
    filepath when extracting.  The equivalent of \command{unzip -j}.}

  \item{exdir}{The directory to extract files to (the equivalent of
    \code{unzip -d}).  It will be created if necessary.}

  \item{unzip}{The method to be used.  An alternative is to use
    \code{getOption("unzip")}, which on a Unix-alike may be set to the
    path to a \command{unzip} program.}

  \item{setTimes}{logical.  For the internal method only, should the
    file time be set based on the times in the zip file?  (NB: this
    applies to files, not to directories.}
}
\note{
  The default internal method is a minimal implementation, principally
  designed for Windows' users to be able to unpack Windows binary
  packages without external software.  It does not (for example) support
  Unicode filenames as introduced in \command{zip 3.0}: for that use
  \code{unzip = "unzip"} with \command{unzip 6.00} or later.  (As from
  \R 2.14.0 it does have support for files of more than 4GB and
  \command{bzip2} compression.)

  If \code{unzip} specifies a program, the format of the dates listed
  with \code{list = TRUE} is unknown (on Windows it can even depend on
  the current locale) and the return values could be \code{NA} or
  expressed in the wrong time zone or misinterpreted (the latter being
  far less likely as from \command{unzip 6.00}).

  File times in zip files are stored in the style of MS-DOS, as local times
  to an accuracy of 2 seconds.  This is not very useful when
  transferring zip files between machines (even across continents), so
  we chose not to restore them by default.
}

\value{
  If \code{list = TRUE}, a data frame with columns \code{Name}
  (character) \code{Length} (the size of the uncompressed file, numeric)
  and \code{Date} (of class \code{"\link{POSIXct}"}).

  Otherwise for the \code{"internal"} method, a character vector of the
  filepaths extracted to, invisibly.
}

\source{
  The internal C code uses \code{zlib} and is in particular based on the
  contributed \samp{minizip} application in the \code{zlib} sources
  (from \url{http://zlib.net}) by Gilles Vollant.
}

\seealso{
  \code{\link{unz}} to read a single component from a zip file.

  \code{\link{zip}}.
}

\keyword{file}
\keyword{utilities}
