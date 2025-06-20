% File src/library/utils/man/tar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2025 R Core Team
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
    compression = c("none", "gzip", "bzip2", "xz", "zstd"),
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
    for the internal method: see the help for \code{\link{gzfile}} for
    possible values.}

  \item{tar}{character string: the path to the command to be used.  If
    the command itself contains spaces it needs to be quoted (e.g., by
    \code{\link{shQuote}}) -- but argument \code{tar} may also contain
    flags separated from the command by spaces.}

  \item{extra_flags}{Any extra flags for an external \command{tar}.}
}

\details{
  This is either a wrapper for a \command{tar} command or uses an
  internal implementation in \R.  The latter is used if \code{tarfile}
  is a connection or if the argument \code{tar} is \code{"internal"} or
  \code{""} (the \sQuote{factory-fresh} default).  Note that whereas
  Unix-alike versions of \R set the environment variable \env{TAR}, its
  value is not the default for this function.

  Argument \code{extra_flags} is passed to an external \command{tar} and
  so is platform-dependent.  Possibly useful values include \option{-h}
  (follow symbolic links, also \option{-L} on some platforms),
  \samp{--acls}, \option{--exclude-backups}, \option{--exclude-vcs} (and
  similar) and on Windows \option{--force-local} (so drives can be
  included in filepaths: this used to be the default for \R on Windows).

  A convenient and robust way to set options for GNU \command{tar} is via
  environment variable \env{TAR_OPTIONS}.  Appending \option{--force-local}
  to \env{TAR} does not work with GNU \command{tar} due to restrictions on
  how some options can be mixed.  The \command{tar} available on Windows 10
  (\I{libarchive}'s \command{bsdtar}) supports drive letters by default.  It
  does not support the \option{--force-local}, but ignores
  \env{TAR_OPTIONS}.

  For GNU \command{tar},
  \option{--format=ustar} forces a more portable format. (The default is
  set at compilation and will be shown at the end of the output from
  \command{tar --help}: for version 1.35 \sQuote{out-of-the-box} it is 
  \option{--format=gnu}, but the manual says the intention is to change
  %% https://www.gnu.org/software/tar/manual/tar.html#Formats
  to \option{--format=posix} which is the same as \code{pax} --
  it was never part of the POSIX standard for \command{tar} and should
  not be used.  However, the intention has been stated now for several
  years without changing the default.)
  For \I{libarchive}'s \command{bsdtar}, \option{--format=ustar} is more
  portable than the default.
  
  %% This uses -T, not supported by Solaris nor Heirloom Toolchest.
  One issue which can cause an external command to fail is a command
  line too long for the system shell: this is worked
  around if the external command is detected to be GNU \command{tar} or
  \I{libarchive} \command{tar} (aka \command{bsdtar}).

  Note that \code{files = '.'} will usually not work with an external
  \command{tar} as that would expand the list of files after
  \code{tarfile} is created.  (It does work with the default internal
  method.)
}

\value{
  The return code from \code{\link{system}} or \code{0} for the internal
  version, invisibly.
}

\section{Portability}{
  The \sQuote{tar} format no longer has an agreed standard!
  \sQuote{Unix Standard Tar} was part of POSIX 1003.1:1998 but has been
  removed in favour of \command{pax}, and in any case many common
  implementations diverged from the former standard.

  Many \R platforms use a version of GNU \command{tar}, but the
  behaviour seems to be changed with each version.  macOS >= 10.6,
  FreeBSD and Windows 10 use \command{bsdtar} from the \I{libarchive}
  project (but for macOS often a quite-old version), and commercial
  Unixes will have their own versions. \command{bsdtar} is available for
  many other platforms (sometimes bundled with \code{libarchive},
  sometimes not) : macOS used to have
  GNU \command{tar} as \command{gnutar} and other platforms,
  e.g.\sspace{}Solaris, have it as \command{gtar}: on a Unix-alike
  \command{configure} will try \command{gnutar} and \command{gtar}
  before \code{tar}.

  %% Heirloom Toolchest (http://heirloom.sourceforge.net/) is
  %% another implementation available on NetBSD, but probably not the default.

  Known problems arise from
  \itemize{
    \item The handling of file paths of more than 100 bytes.  These were
    unsupported in early versions of \command{tar}, and supported in one
    way by POSIX \command{tar} and in another by GNU \command{tar} and
    yet another by the POSIX \command{pax} command which
    recent \command{tar} programs often support.  The internal
    implementation warns on paths of more than 100 bytes,
    uses the \sQuote{\I{ustar}} way from the 1998 POSIX
    standard which supports up to 256 bytes (depending on the path: in
    particular the final component is limited to 100 bytes) if possible,
    otherwise the GNU way (which is widely supported, including by
    \code{\link{untar}}).

    Most formats do not record the encoding of file paths.

    \item (File) links.  \command{tar} was developed on an OS that used
    hard links, and physical files that were referred to more than once
    in the list of files to be included were included only once, the
    remaining instances being added as links.  Later a means to include
    symbolic links was added.  The internal implementation supports
    symbolic links (on OSes that support them), only.  Of course, the
    question arises as to how links should be unpacked on OSes that do
    not support them: for regular files file copies can be used.

    Names of links in the \sQuote{\I{ustar}} format are restricted to 100
    bytes.  There is an GNU extension for arbitrarily long link names,
    but \command{bsdtar} ignores it.  The internal method uses the
    GNU extension, with a warning.

    \item Header fields, in particular the padding to be used when
    fields are not full or not used.  POSIX did define the correct
    behaviour but commonly used implementations did (and still do)
    not comply.

    \item File sizes.  The \sQuote{\I{ustar}} format is restricted to 8GB
    per (uncompressed) file.
  }
  For portability, avoid file paths of more than 100 bytes and all links
  (especially hard links and symbolic links to directories).

  The internal implementation writes only the blocks of 512 bytes
  required (including trailing blocks of \abbr{NUL}s), unlike GNU \command{tar}
  which by default pads with \samp{nul} to a multiple of 20 blocks
  (10KB).  Implementations which pad differ on whether the block padding
  should occur before or after compression (or both): padding was
  designed for improved performance on physical tape drives.
  
  The \sQuote{\I{ustar}} format records file modification times to a
  resolution of 1 second: on file systems with higher resolution it is
  conventional to discard fractional seconds.
}

\section{Compression}{
  When an external \command{tar} command is used, compressing the tar
  archive requires that \command{tar} supports the \option{-z},
  \option{-j}, \option{-J} or \option{--zstd}flag, and may require the
  appropriate command (\command{gzip}, \command{bzip2} \command{xz} or
  \command{zstd}) to be available.  For GNU \command{tar}, further
  compression programs can be specified by
  e.g.\sspace{}\code{extra_flags = "-I lz4"} or \code{"--lzip"} or
  \code{"--lzop"} in argument \code{extra_flags}.  Some versions of
  \command{bsdtar} accept options such as \option{--lz4},
  \option{--lzop} and \option{--lrzip} or an external compressor
  \emph{via} \option{--use-compress-program lz4}: these could be
  supplied in \code{extra_flags}.
  
  \I{NetBSD} prior to 8.0 used flag \option{--xz} rather than \option{-J},
  so this should be used \emph{via} \code{extra_flags = "--xz"} rather
  than \code{compression = "xz"}.  The commands from \I{OpenBSD} and the
  \I{Heirloom Toolchest} are not documented to support \command{xz} nor
  \command{zstd}.

  The \command{tar} program in recent macOS (e.g.\sspace{}15.5) does
  support \command{zstd} compression.\emph{via} an external
  \command{zstd} program, but Apple does not supply one.

  %% The \command{tar} programs in commercial Unixen such as \I{AIX} and
  %% Solaris do not support compression.

  GNU \command{tar} added support in version 1.22 for \command{xz}
  compression and in version 1.31 for \command{zstd} compression.
  \command{bsdtar} added support for \command{xz} in 2019 and for
  \command{zstd} in 2020.

  Neither the internal or the known external \command{tar} commands
  support parallel compression --- but this function can be used to write
  an uncompressed tarball which can then be compressed in parallel, for
  example with \command{zstd -T0}.
}

\note{
  For users of macOS.  Apple's legacy file systems had a concept of
  \sQuote{resource forks} dating from classic Mac OS and rarely used
  nowadays (and only partially supported in APFS).  Apple's version of
  \command{tar} stored these as separate files in the tarball with names
  prefixed by \file{._}, and unpacks such files into resource forks (if
  possible): other ways of unpacking (including \code{\link{untar}} in
  \R) unpack them as separate files.

  When argument \code{tar} is set to the command \command{tar} on macOS,
  environment variable \env{COPYFILE_DISABLE=1} is set, which for the
  system version of \command{tar} prevents these separate files being
  included in the tarball.
}

\seealso{
  \url{https://en.wikipedia.org/wiki/Tar_(file_format)},
  \url{https://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_06}
  for the way the POSIX utility \command{pax} handles \command{tar} formats.

  \url{https://github.com/libarchive/libarchive/wiki/FormatTar}.

  \code{\link{untar}}.
}

\keyword{file}
\keyword{utilities}
