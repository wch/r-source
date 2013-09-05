% File src/library/base/man/list.files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{list.files}
\alias{list.files}
\alias{dir}
\alias{list.dirs}
\title{List the Files in a Directory/Folder}
\usage{
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

       dir(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
}
\arguments{
  \item{path}{a character vector of full path names; the default
    corresponds to the working directory, \code{\link{getwd}()}.  Tilde
    expansion (see \code{\link{path.expand}}) is performed.  Missing
    values will be ignored.}
  \item{pattern}{an optional \link{regular expression}.  Only file names
    which match the regular expression will be returned.}
  \item{all.files}{a logical value.  If \code{FALSE}, only the
    names of visible files are returned.  If \code{TRUE},
    all file names will be returned.}
  \item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names to give a relative file path.
    If \code{FALSE}, the file names (rather than paths) are returned.}
  \item{recursive}{logical.  Should the listing recurse into directories?}
  \item{ignore.case}{logical.  Should pattern-matching be case-insensitive?}
  \item{include.dirs}{logical.  Should subdirectory names be included in
    recursive listings?  (They always are in non-recursive ones).}
  \item{no..}{logical.  Should both \code{"."} and \code{".."} be excluded
    also from non-recursive listings?}
}
\description{
  These functions produce a character vector of the names of files or
  directories in the named directory.
}
\value{
  A character vector containing the names of the files in the
  specified directories, or \code{""} if there were no files.  If a
  path does not exist or is not a directory or is unreadable it
  is skipped, with a warning.

  The files are sorted in alphabetical order, on the full path
  if \code{full.names = TRUE}.

  \code{list.dirs} implicitly has \code{all.files = TRUE}, and if
  \code{recursive = TRUE}, the answer includes \code{path} itself
  (provided it is a readable directory).
}
\author{Ross Ihaka, Brian Ripley}
\note{
  File naming conventions are platform dependent.  The pattern matching
  works with the case of file names as returned by the OS.
#ifdef unix

  On a POSIX filesystem recursive listings will follow symbolic links to
  directories.
#endif
#ifdef windows

  \code{path} must specify paths which can be represented in the current
  codepage.
#endif
}
\seealso{\code{\link{file.info}}, \code{\link{file.access}}
  and \code{\link{files}} for many more file handling functions and
  \code{\link{file.choose}}
#ifdef windows
  and \code{\link{choose.files}}
#endif
  for interactive selection.

  \code{\link{glob2rx}} to convert wildcards (as used by system file
  commands and shells) to regular expressions.

  \code{\link{Sys.glob}} for wildcard expansion on file paths.
}
\examples{
list.files(R.home())
## Only files starting with a-l or r
## Note that a-l is locale-dependent, but using case-insensitive
## matching makes it unambiguous in English locales
dir("../..", pattern = "^[a-lr]", full.names = TRUE, ignore.case = TRUE)

list.dirs(R.home("doc"))
list.dirs(R.home("doc"), full.names = FALSE)
}
\keyword{file}
