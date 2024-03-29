% File src/library/utils/man/normalizePath.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{normalizePath}
\alias{normalizePath}
\title{Express File Paths in Canonical Form}
\description{
  Convert file paths to canonical form for the platform, to display them
  in a user-understandable form and so that relative and absolute paths can
  be compared.
}
\usage{
normalizePath(path, winslash = "\\\\", mustWork = NA)
}
\arguments{
  \item{path}{character vector of file paths.}
  \item{winslash}{the separator to be used on Windows -- ignored
    elsewhere.  Must be one of \code{c("/", "\\\\")}.}
  \item{mustWork}{logical: if \code{TRUE} then an error is given if the result
    cannot be determined; if \code{NA} then a warning.}
}
\details{
  Tilde-expansion (see \code{\link{path.expand}}) is first done on
  \code{paths}.

  Where the Unix-alike platform supports it attempts to turn paths into
  absolute paths in their canonical form (no \samp{./}, \samp{../} nor
  symbolic links).  It relies on the POSIX system function
  \code{realpath}: if the platform does not have that (we know of no
  current example) then the result will be an absolute path but might
  not be canonical.  Even where \code{realpath} is used the canonical
  path need not be unique, for example \emph{via} hard links or
  multiple mounts.

  On Windows it converts relative paths to absolute paths, resolves symbolic
  links, converts short names for path elements to long names and ensures the
  separator is that specified by \code{winslash}.  It will match each path
  element case-insensitively or case-sensitively as during the usual name
  lookup and return the canonical case. It relies on Windows API function
  \code{GetFinalPathNameByHandle} and in case of an error (such as
  insufficient permissions) it currently falls back to the \R 3.6 (and
  older) implementation, which relies on \code{GetFullPathName} and
  \code{GetLongPathName} with limitations described in the Notes section.
  An attempt is made not to introduce \abbr{UNC} paths in presence of mapped drives
  or symbolic links: if \code{GetFinalPathNameByHandle} returns a \abbr{UNC} path,
  but \code{GetLongPathName} returns a path starting with a drive letter, R
  falls back to the \R 3.6 (and older) implementation.
  UTF-8-encoded paths not valid in the current locale can be used.

  \code{mustWork = FALSE} is useful for expressing paths for use in
  messages.
}

\note{
  The canonical form of paths may not be what you expect.  For example,
  on macOS absolute paths such as \file{/tmp} and \file{/var} are
  symbolic links. On Linux, a path produced by bash process substitution is
  a symbolic link (such as \file{/proc/fd/63}) to a pipe and there is no
  canonical form of such path. In \R 3.6 and older on Windows, symlinks will
  not be resolved and the long names for path elements will be returned with
  the case in which they are in \code{path}, which may not be canonical in
  case-insensitive folders.
}

\value{
  A character vector.

  If an input is not a real path the result is system-dependent (unless
  \code{mustWork = TRUE}, when this should be an error).  It will be
  either the corresponding input element or a transformation of it into
  an absolute path.

  Converting to an absolute file path can fail for a large number of
  reasons.  The most common are
  \itemize{
    \item One of more components of the file path does not exist.
    \item A component before the last is not a directory, or there is
    insufficient permission to read the directory.
    \item For a relative path, the current directory cannot be
    determined.
    \item A symbolic link points to a non-existent place or links form a
    loop.
    \item The canonicalized path would be exceed the maximum supported
    length of a file path.
  }
}

#ifdef windows
\seealso{
  \code{\link{shortPathName}}
}
#endif

\examples{\dontdiff{% random tempdir
cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
}}
\keyword{ utilities }
