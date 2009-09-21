% File src/library/utils/man/tar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{tar}
\alias{tar}
\title{
  Create a Tar Archive
}
\description{
  Create a tar archive.
}
\usage{
tar(tarfile, files = NULL,
    compression = c("none", "gzip", "bzip2", "xz"),
    compression_level = 6)
}

\arguments{
  \item{tarfile}{The pathname of the tar file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.  Alternatively, a
    connection that can be used for binary writes.}

  \item{files}{A character vector of filepaths to be archived:
    the default is to archive all files under the current directory.}
  
  \item{compression}{logical or character.  The type of compression to
    be used.  Can be abbreviated.}

  \item{compression_level}{integer: the level of compression.  Only used
    for the internal method.}
}

\details{
  This is either a wrapper for a \command{tar} command or uses an
  internal implementation in \R.  The latter is used if \code{tarfile}
  is a connection or if the environment variable \env{TAR} is
  \code{"internal"} or unset.

  The internal implementation writes only the blocks of 512 bytes
  required, unlike GNU \command{tar} which by default pads with
  \samp{nul} to a multiple of 20 blocks (10KB).
}

\value{
  The return code from \code{\link{system}}, invisibly.
}

\seealso{
  \url{http://en.wikipedia.org/wiki/Tar_(file_format)},
  \url{http://www.opengroup.org/onlinepubs/009695399/utilities/pax.html#tag_04_100_13_06}.
  
  \code{\link{untar}}.
}

\keyword{file}
\keyword{utilities}
