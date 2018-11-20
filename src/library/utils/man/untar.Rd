% File src/library/utils/man/untar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2018 R Core Team
% Distributed under GPL 2 or later

\name{untar}
\alias{untar}
\title{
  Extract or List Tar Archives
}
\description{
  Extract files from or list the contents of a tar archive.
}
\usage{
untar(tarfile, files = NULL, list = FALSE, exdir = ".",
      compressed = NA, extras = NULL, verbose = FALSE,
      restore_times =  TRUE,
      support_old_tars = Sys.getenv("R_SUPPORT_OLD_TARS", FALSE),
      tar = Sys.getenv("TAR"))
}
\arguments{
  \item{tarfile}{The pathname of the tar file: tilde expansion (see
    \code{\link{path.expand}}) will be performed.  Alternatively, a
    \link{connection} that can be used for binary reads.  For a
    \emph{compressed} \code{tarfile}, and if a connection is to be used,
    that should be created by \code{\link{gzfile}(.)} (or
    \code{\link{gzcon}(.)} which currently only works for \code{"gzip"},
    whereas \code{gzfile()} works for all compressions available in
    \code{\link{tar}()}).}

  \item{files}{A character vector of recorded filepaths to be extracted:
    the default is to extract all files.}

  \item{list}{If \code{TRUE}, list the files (the equivalent of
    \command{tar -tf}).  Otherwise extract the files (the equivalent of
    \command{tar -xf}).}

  \item{exdir}{The directory to extract files to (the equivalent of
    \command{tar -C}).  It will be created if necessary.}

  \item{compressed}{(Deprecated in favour of auto-detection.)  Logical
    or character string, used only for an external \command{tar}
    command.  Values \code{"gzip"}, \code{"bzip2"} and \code{"xz"}
    select that form of compression (and may be abbreviated to the first
    letter).  \code{TRUE} indicates \command{gzip} compression,
    \code{FALSE} no known compression, and \code{NA} (the default)
    indicates that the type is inferred from the file header.

    The external command may ignore the selected compression type but
    detect a type automagically.
  }

  \item{extras}{\code{NULL} or a character string: further command-line
    flags such as \option{-p} to be passed to an external \command{tar}
    program.}

  \item{verbose}{logical: if true echo the command used for an external
    \command{tar} program.}

  \item{restore_times}{logical.  If true (default) restore file
    modification times.  If false, the equivalent of the \option{-m}
    flag.  Times in tarballs are supposed to be in UTC, but tarballs
    have been submitted to CRAN with times in the future or far past:
    this argument allows such times to be discarded.

    Note that file times in a tarball are stored with a resolution of 1
    second, and can only be restored to the resolution supported by the
    file system (which on a FAT system is 2 seconds).
  }
  
  %% even RHEL6 had GNU tar 1.23, Ububtu 14.04 has 1.27
  %% macOS has bsdtar from 2010: this supports xz but does not docuemnt it.
  %% bsdtar had lzma/xz supoort before April 2009 when it added
  %% support for command-line unxz etc, not just libraries.
  \item{support_old_tars}{logical.  If true, the external \command{tar}
    command is assumed to be able handle compressed tarfiles and if
    \code{compressed} does not specify it, to automagically detect the
    type of compression.  (The major implementations have done so since
    2009; for GNU \command{tar} since version 1.22.)

    Otherwise the \R code calls an appropriate decompressor and pipes
    the output to \command{tar}, for \code{compressed = NA} examining
    the tarfile header to determine the type of compression.
  }

  \item{tar}{character string: the path to the command to be used or
    \code{"internal"}.  If the command itself contains spaces it needs
    to be quoted -- but \code{tar} can also contain flags separated from
    the command by spaces.}
}

\details{
  This is either a wrapper for a \command{tar} command or for an
  internal implementation written in \R.  The latter is used if
  \code{tarfile} is a connection or if the argument \code{tar} is
  \code{"internal"} or \code{""} (except on Windows, when
  \command{tar.exe} is tried first).
  
  Unless otherwise stated three types of compression of the tar file are
  supported: \command{gzip}, \command{bzip2} and \command{xz}.
  
  What options are supported will depend on the \command{tar}
  implementation used: the \code{"internal"} one is intended to provide
  support for most in a platform-independent way.
  \describe{
    \item{GNU tar:}{Modern GNU \command{tar} versions support
      compressed archives and since 1.15 are able to detect the type of
      compression automatically: version 1.22 added support for
      \command{xz} compression.

      On a Unix-alike, \command{configure} will set environment variable
      \env{TAR}, preferring GNU tar if found.}

    %% bsdtar had it in FreeBSB 5.3 (2004)
    \item{\code{bsdtar}:}{macOS 10.6 and later (and FreeBSD and some
      other OSes) have a \command{tar}
      from the libarchive project which detects all three forms
      of compression automagically (even if undocumented in macOS).}

    \item{NetBSD:}{It is undocumented if NetBSD's \command{tar} can
      detect compression automagically: for versions before 8 the flag
      for \command{xz} compression was \option{--xz} not \option{-J}.
      So \code{support_old_tars = TRUE} is recommended (or use
      \command{bsdtar} if installed).}

    %% https://news.ycombinator.com/item?id=10722697
    \item{OpenBSD:}{OpenBSD's \command{tar} does not detect compression
      automagically.  It has no support for \command{xz} beyond reporting
      that the file is \command{xz}-compressed.  So \code{support_old_tars
	= TRUE} is recommended.}

    \item{Heirloom Toolchest:}{This \command{tar} does automagically
      detect \command{gzip} and \command{bzip2} compression (undocumented)
      but has no support for \command{xz} compression.}

    \item{Older support:}{Environment variable \env{R_GZIPCMD} gives the
      command to decompress \command{gzip} files, and
      \env{R_BZIPCMD} for \command{bzip2} files. (On Unix-alikes
      these are set at installation if found.)  \command{xz} is used if
      available: if not decompression is expected to fail.}
  }

  Arguments \code{compressed}, \code{extras} and \code{verbose} are only
  used when an external \command{tar} is used.

  Some external \command{tar} commands will detect \command{lrzip},
  \command{lzma}, \command{lz4}, \command{lzop} or \command{zstd}
  compression in addition to \command{gzip}, \command{bzip2} and
  \command{xz}.  (For some external \command{tar} commands, compressed
  tarfiles can only be read if the appropriate utility program is
  available.)  For GNU \command{tar}, further (de)compression programs
  can be specified by e.g.\sspace{}\code{extras = "-I lz4"}.
  For \command{bsdtar} this could be
  \code{extras = "--use-compress-program lz4"}.
  Most commands will detect (the nowadays rarely seen) \file{.tar.Z}
  archives compressed by \code{compress}.
  
  The internal implementation restores symbolic links as links on a
  Unix-alike, and as file copies on Windows (which works only for
  existing files, not for directories), and hard links as links.  If the
  linking operation fails (as it may on a FAT file system), a file copy
  is tried.  Since it uses \code{\link{gzfile}} to read a file it can
  handle files compressed by any of the methods that function can
  handle: at least \command{compress}, \command{gzip}, \command{bzip2}
  and \command{xz} compression, and some types of \command{lzma}
  compression.  It does not guard against restoring absolute file paths,
  as some \command{tar} implementations do.  It will create the parent
  directories for directories or files in the archive if necessary.  It
  handles the standard (USTAR/POSIX), GNU and \command{pax} ways of
  handling file paths of more than 100 bytes, and the GNU way of
  handling link targets of more than 100 bytes.

  You may see warnings from the internal implementation such
  as \preformatted{    unsupported entry type 'x'}
  This often indicates an invalid archive: entry types \code{"A-Z"} are
  allowed as extensions, but other types are reserved.  The only thing
  you can do with such an archive is to find a \code{tar} program that
  handles it, and look carefully at the resulting files.  There may also
  be the warning \preformatted{    using pax extended headers}
  This indicates that additional information may have been discarded,
  such as ACLs, encodings \dots.

  The standards only support ASCII filenames (indeed, only alphanumeric
  plus period, underscore and hyphen).  \code{untar} makes no attempt to
  map filenames to those acceptable on the current system, and treats
  the filenames in the archive as applicable without any re-encoding in
  the current locale.

  The internal implementation does not special-case \sQuote{resource
  forks} in macOS: that system's \command{tar} command does. This may
  lead to unexpected files with names with prefix \file{._}.
}

\value{
  If \code{list = TRUE}, a character vector of (relative or absolute)
  paths of files contained in the tar archive.

  Otherwise the return code from \code{\link{system}} with an external
  \command{tar} or \code{0L}, invisibly.
}

\seealso{
  \code{\link{tar}}, \code{\link{unzip}}.
}

\keyword{file}
\keyword{utilities}
