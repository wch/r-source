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
  This is either a wrapper for a \command{tar} command or for an
  internal \R implementation of tar.  The \R implementation is used if
  \code{tarfile} is a connection or if the environment variable
  \env{TAR} is \code{"internal"} or unset.

  The internal implementation only handles regular files and directories
  (and not links on OSes which support them).
}

\value{
  The return code from \code{\link{system}}, invisibly.
}

\seealso{
  \url{http://en.wikipedia.org/wiki/Tar_(file_format)}.
  
  \code{\link{untar}}.
}

\keyword{file}
\keyword{utilities}
