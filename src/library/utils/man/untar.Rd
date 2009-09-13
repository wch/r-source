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
      compressed = NA, preserve = FALSE, verbose = FALSE)
}
\arguments{
  \item{tarfile}{The pathname of the zip file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}
  
  \item{list}{If \code{TRUE}, list the files and extract none.  The
    equivalent of \command{unzip -l}.}
    
  \item{exdir}{The directory to extract files to (the equivalent of
    \code{unzip -d}).  It will be created if necessary.}

  \item{compressed}{logical or character.  Values \code{"gzip"} and
    \code{"bzip2"} select that form of compression (and may be
    abbreviated to the first letter).  \code{TRUE} indicates, gzip
    compression, \code{FALSE} no compression (but the \command{tar}
    command may detect this automagically), and \code{NA} (the default)
    that the type is inferred from the file name: an extension
    \file{bz2} selects \command{bzip2} compression and any of \file{gz},
    \file{tgz}, \file{taz} or \file{Z} selects \command{gzip}
    compression.}

  \item{preserve}{logical: should the \code{p} modifier be used to
    restore file permissions (on OSes where this makes sense).}

  \item{verbose}{logical: if true echo the command used.}
}
\details{
  This is a wrapper for a \command{tar} or \command{untgz} command,
  selected by the environment variable \env{TAR}.  This is set at
  installation on a Unix-alike, and on Windows to \file{untgz.exe} (which
  we supply) unless \file{tar.exe} is in the path.

  What options are supported will depend on the \command{tar} used.  It
  is likely that only GNU flavours of \command{tar} will support
  compressed archives, and recent versions will be able to detect the
  type of compression automatically: version 1.22 added support for
  \command{xz} and \command{lzma} compression.
#ifdef windows
  
  \file{untgz.exe} supports only uncompressed and
  \command{gzip}-compressed archives (including \file{.Z}, which
  \command{gzip} can handle).
#endif
}

\value{
  If \code{list = TRUE}, a character vector of (relative or absolute)
  paths of files contained in the tar archive.

  Otherwise the return code from \code{\link{system}}, invisibly.
}

\seealso{
  \code{\link{unzip}}
}

\keyword{file}
\keyword{utilities}
