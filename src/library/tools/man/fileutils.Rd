\name{fileutils}
\alias{filePathAsAbsolute}
\alias{filePathSansExt}
\alias{fileTest}
\alias{listFilesWithExts}
\alias{listFilesWithType}
\title{File Utilities}
\description{
  Utilities for testing and listing files, and manipulating file paths.
}
\usage{
filePathAsAbsolute(x)
filePathSansExt(x)
fileTest(op, x, y)
listFilesWithExts(dir, exts, all.files = FALSE, full.names = TRUE)
listFilesWithType(dir, type, all.files = FALSE, full.names = TRUE)
}
\arguments{
  \item{x,y}{character vectors giving file paths.}
  \item{op}{a character string specifying the test to be performed.
    Unary tests (only \code{x} is used) are \code{"-f"} (existence and
    not being a directory) and \code{"-d"} (existence and directory);
    binary tests are \code{"-nt"} (newer than, using the modification
    dates) and \code{"-ot"}.}
  \item{dir}{a character string with the path name to a directory.}
  \item{exts}{a character vector of possible file extensions.}
  \item{all.files}{a logical.  If \code{FALSE} (default), only visible
    files are considered; if \code{TRUE}, all files are used.}
  \item{full.names}{a logical indicating whether the full paths of the
    files found are returned (default), or just the file names.}
  \item{type}{a character string giving the \dQuote{type} of the files
    to be listed, as characterized by their extensions.  Currently,
    possible values are \code{"code"} (R code), \code{"data"} (data
    sets), \code{"demo"} (demos), \code{"docs"} (R documentation), and
    \code{"vignette"} (vignettes).}
}
\details{
  \code{filePathAsAbsolute} turns a possibly relative file path
  absolute, performing tilde expansion if necessary.  Currently, only a
  single existing path can be given.

  \code{filePathSansExt} returns the file paths without extensions.
  (Only purely alphanumeric extensions are recognized.)

  \code{fileTest} performs shell-style file tests.  Note that
  \code{\link{file.exists}} only tests for existence (\code{test -e} on
  some systems) but not for not being a directory.

  \code{listFilesWithExts} returns the paths or names of the files in
  directory \code{dir} with extension matching one of the elements of
  \code{exts}.  Note that by default, full paths are returned, and that
  only visible files are used.

  \code{listFilesWithType} returns the paths of the files in \code{dir}
  of the given \dQuote{type}, as determined by the extensions recognized
  by \R.  When listing R code and documentation files, files in
  OS-specific subdirectories are included if present.  Note that by
  default, full paths are returned, and that only visible files are
  used.
}
\seealso{
  \code{\link{file.path}},
  \code{\link{file.info}},
  \code{\link{list.files}}
}
\examples{
dir <- file.path(R.home(), "library", "stats")
fileTest("-d", dir)
fileTest("-nt", file.path(dir, "R"), file.path(dir, "demo"))
listFilesWithExts(file.path(dir, "demo"), "R")
listFilesWithType(file.path(dir, "demo"), "demo") # the same
filePathSansExt(list.files(file.path(R.home(), "modules")))
}
\keyword{file}
