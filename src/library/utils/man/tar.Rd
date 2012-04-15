% File src/library/utils/man/tar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Team
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
    compression_level = 6, tar = Sys.getenv("tar"),
    extra_flags = "")
}

\arguments{
  \item{tarfile}{The pathname of the tar file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.  Alternatively, a
    \link{connection} that can be used for binary writes.}

  \item{files}{A character vector of filepaths to be archived:
    the default is to archive all files under the current directory.}
  
  \item{compression}{character string giving the type of compression to
    be used (default none).  Can be abbreviated.}

  \item{compression_level}{integer: the level of compression.  Only used
    for the internal method.}

  \item{tar}{character string: the path to the command to be used.  If
    the command itself contains spaces it needs to be quoted -- but
    \code{tar} can also contain flags separated from the command by
    spaces.}

  \item{extra_flags}{any extra flags for an external \command{tar}.}
}

\details{
  This is either a wrapper for a \command{tar} command or uses an
  internal implementation in \R.  The latter is used if \code{tarfile}
  is a connection or if the argument \code{tar} is \code{"internal"} or
  \code{""}.  Note that whereas Unix-alike versions of \R set the
  environment variable \env{TAR}, its value is not the default for this
  function.

  Argument \code{extra_flags} is passed to an external \command{tar} and
  so is platform-dependent.  Possibly useful values include \option{-h}
  (follow symbolic links, also \option{-L} on some platforms),
  \samp{--acls}, \option{--exclude-backups}, \option{--exclude-vcs} (and
  similar) and on Windows \option{--force-local} (so drives can be
  included in filepaths: this is the default for the \command{Rtools}
  \command{tar}.
}

\section{Portability}{
  The \sQuote{tar} format no longer has an agreed standard!
  \sQuote{Unix Standard Tar} was part of POSIX 1003.1:1998 but has been
  removed in favour of \command{pax}, and in any case many common
  implementations diverged from the former standard.  Most \R platforms
  use a version of GNU \command{tar} (including \command{Rtools} on
  Windows, but the behaviour seems to be changed with each version), Mac
  OS 10.6 and FreeBSD use \command{bsdttar} from the \samp{libarchive
  project}, and commercial Unixes will have their own versions.

  Known problems arise from
  \itemize{
    \item The handling of file names of more than 100 bytes.  These were
    unsupported in early versions of \command{tar}, and supported in one
    way by POSIX \command{tar} and in another by GNU \command{tar}.  The
    internal implementation uses the POSIX way which supports up to 255
    bytes (depending on the path), and warns on paths of more than 100
    bytes.

    \item (File) links.  \command{tar} was developed on an OS that used
    hard links, and physical files that were referred to more than one
    in the list of files to be included were included only once, the
    remaining instance being added as links.  Later a means to include
    symbolic links was added.  The internal implementation supports
    symbolic links (on OSes that support them), only.  Of course, the
    question arises as to how links should be unpacked on OSes that do
    not support them: for files at least file copies can be used.

    \item Header fields, in particular the padding to be used when
    fields are not full or not used.  POSIX did define the correct
    behaviour but commonly used implementations did (and still do)
    not comply.
  }
  For portability, avoid file paths of more than 100 bytes, and links
  (or at least, hard links and symbolic links to directories).

  The internal implementation writes only the blocks of 512 bytes
  required, unlike GNU \command{tar} which by default pads with
  \samp{nul} to a multiple of 20 blocks (10KB).  Implementations differ
  on whether the block padding should occur before or after compression
  (or both).
}

\value{
  The return code from \code{\link{system}} or \code{0} for the internal
  version, invisibly.
}

\seealso{
  \url{http://en.wikipedia.org/wiki/Tar_(file_format)},
  \url{http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_06}
  for the way the POSIX utility \command{pax} handles \command{tar} formats.

  \url{http://code.google.com/p/libarchive/wiki/ManPageTar5}.
  
  \code{\link{untar}}.
}

\keyword{file}
\keyword{utilities}
