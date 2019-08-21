% File src/library/base/man/UTFfilepaths.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2019 R Core Team
% Distributed under GPL 2 or later

\name{UTF8filepaths}
\alias{file path encoding}
\alias{UTF-8 file path}
\title{File Paths not in the Native Encoding}

\description{
  Most modern file systems store file-path components (names of
  directories and files) in a character encoding of wide scope: usually
  UTF-8 on a Unix-alike and UCS-2/UTF-16 on Windows.  However, this was
  not true when \R was first developed and there are still exceptions
  amongst file systems, e.g.\sspace{}FAT32.

  This was not something anticipated by the C and POSIX standards which
  only provide means to access files \emph{via} file paths encoded in
  the current locale, for example those specified in Latin-1 in a
  Latin-1 locale.

  Everything here apart from the specific section on Windows is about
  Unix-alikes.
}

\details{
  It is possible to mark character strings (elements of character
  vectors) as being in UTF-8 or Latin-1 (see \code{\link{Encoding}}).
  This allows file paths not in the native encoding to be
  expressed in \R character vectors but there is almost no way to use
  them unless they can be translated to the native encoding.  That is of
  course not a problem if that is UTF-8, so these details are really only
  relevant to the use of a non-UTF-8 locale (including a C locale) on a
  Unix-alike.

  Functions to open a file such as \code{\link{file}},
  \code{\link{fifo}}, \code{\link{pipe}}, \code{\link{gzfile}},
  \code{\link{bzfile}}, \code{\link{xzfile}} and \code{\link{unz}} give
  an error for non-native filepaths.  Where functions look at existence
  such as \code{file.exists}, \code{\link{dir.exists}},
  \code{\link{unlink}}, \code{\link{file.info}} and
  \code{\link{list.files}}, non-native filepaths are treated as
  non-existent.

  Many other functions use \code{file} or \code{gzfile} to open their
  files.

  \code{\link{file.path}} allows non-native file paths to be combined,
  marking them as UTF-8 if needed.

  \code{\link{path.expand}} only handles paths in the native encoding.  
}

\section{Windows}{
  Windows provides proprietary entry points to access its file systems,
  and these gained \sQuote{wide} versions in Windows NT that allowed
  file paths in UCS-2/UTF-16 to be accessed from any locale.

  Some \R functions use these entry points when file paths are marked
  as Latin-1 or UTF-8 to allow access to paths not in the current
  encoding.  These include
  %
  \code{\link{file}}, \code{\link{file.access}},
  \code{\link{file.append}}, \code{\link{file.copy}},
  \code{\link{file.create}}, \code{\link{file.exists}},
  \code{\link{file.info}}, \code{\link{file.link}},
  \code{\link{file.remove}}, \code{\link{file.rename}},
  \code{\link{file.symlink}}
  %
  and
  %
  \code{\link{dir.create}}, \code{\link{dir.exists}},
  \code{\link{normalizePath}}, \code{\link{path.expand}},
  \code{\link{pipe}}, \code{\link{Sys.glob}},
#ifdef unix
  \code{Sys.junction},
#endif
#ifdef windows
  \code{\link{Sys.junction}},
#endif
  \code{\link{unlink}}
  %
   but not \code{\link{gzfile}} (used by \code{\link{load}},
   \code{\link{readRDS}}, \code{\link{read.dcf}} and \code{\link{tar}}),
   \code{\link{bzfile}}, \code{\link{xzfile}} nor \code{\link{unz}}.

  Other notable exceptions are \code{\link{list.files}},
  \code{\link{list.dirs}}, \code{\link{system}} and file-path inputs for
  graphics devices.
}

\section{Historical comment}{
  Before \R 3.7.0, file paths marked as being in Latin-1 or UTF-8 were
  silently translated to the native encoding using escapes such as
  \samp{<e7>} or \samp{<U+00e7>}.  This created valid file names but
  maybe not those intended.
}

\note{
  This document is still a work-in-progress.
}

\keyword{file}

