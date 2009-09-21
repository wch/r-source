% File src/library/utils/man/untar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{untar}
\alias{untar}
\title{
  Extract or List Tar Archives
}
\description{
  Extract files from or list a tar archive.
}
\usage{
untar(tarfile, files = NULL, list = FALSE, exdir = ".",
      compressed = NA, extras = NULL, verbose = FALSE)
}
\arguments{
  \item{tarfile}{The pathname of the tar file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.  Alternatively, a
    connection that can be used for binary reads.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}
  
  \item{list}{If \code{TRUE}, list the files and extract none.  The
    equivalent of \command{tar -tf}.}
    
  \item{exdir}{The directory to extract files to (the equivalent of
    \code{tar -C}).  It will be created if necessary.}

  \item{compressed}{logical or character.  Values \code{"gzip"} and
    \code{"bzip2"} select that form of compression (and may be
    abbreviated to the first letter).  \code{TRUE} indicates gzip
    compression, \code{FALSE} no compression (but the \command{tar}
    command may detect this automagically), and \code{NA} (the default)
    that the type is inferred from the file name: an extension
    \file{bz2} selects \command{bzip2} compression and any of \file{gz},
    \file{tgz}, \file{taz} or \file{Z} selects \command{gzip}
    compression.}

  \item{extras}{\code{NULL} or a character string: further command-ilne
    flags such as \option{-p} to be passed to the \command{tar} program.}

  \item{verbose}{logical: if true echo the command used.}
}
\details{
  This is either a wrapper for a \command{tar} command or for an \R
  implementation of untar.  The \R implementation is used if
  \code{tarfile} is a connection or if the environment variable \env{TAR}
  is \code{"internal"} or unset (except on Windows, when
  \command{tar.exe} is tried first).

  What options are supported will depend on the \command{tar} used.
  Modern GNU flavours of \command{tar} will support compressed archives,
  and since 1.15 are able to detect the type of compression
  automatically: version 1.20 added support for \command{lzma} and
  version 1.22 for \command{xz} compression using LZMA2.  For other
  flavours environment variable \env{R_ZIPCMD} gives the command to
  decompress \command{gzip} files, and \command{bzip2} for its files.
  (There is a \command{bsdtar} command from the \samp{libarchive}
  project used by Mac OS 10.6 (\sQuote{Snow Leopard})which can also
  detect compression automagically as can the \command{tar} from the
  \sQuote{Heirloom Toolchest}.)
  
  Arguments \code{compressed}, \code{extras} and \code{verbose} are only
  used when an external \command{tar} is used.

  The internal implementation restores links (hard and soft) as symbolic
  links on a Unix-alike, and as file copies on Windows.  Since it uses
  \code{\link{gzfile}} to read a file it can handle files compressed by
  any of the methods that \code{gzfile} can handle: at least
  \command{gzip} and \command{bzip2} compression, and \command{xz} on
  some \R builds (see \code{\link{capabilities}("xz")}).
}

\value{
  If \code{list = TRUE}, a character vector of (relative or absolute)
  paths of files contained in the tar archive.

  Otherwise the return code from \code{\link{system}}, invisibly.
}

\seealso{
  \url{http://en.wikipedia.org/wiki/Tar_(file_format)}.
  
  \code{\link{unzip}}.  
}

\keyword{file}
\keyword{utilities}
