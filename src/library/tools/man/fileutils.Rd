% File src/library/tools/man/fileutils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{fileutils}
\alias{file_ext}
\alias{file_path_as_absolute}
\alias{file_path_sans_ext}
\alias{list_files_with_exts}
\alias{list_files_with_type}
\title{File Utilities}
\description{
  Utilities for listing files, and manipulating file paths.
}
\usage{
file_ext(x)
file_path_as_absolute(x)
file_path_sans_ext(x, compression = FALSE)

list_files_with_exts(dir, exts, all.files = FALSE,
                     full.names = TRUE)
list_files_with_type(dir, type, all.files = FALSE,
                     full.names = TRUE, OS_subdirs = .OStype())
}
\arguments{
  \item{x}{character vector giving file paths.}
  \item{compression}{logical: should compression extension \file{.gz},
    \file{.bz2} or \file{.xz} be removed first?}
  \item{dir}{a character string with the path name to a directory.}
  \item{exts}{a character vector of possible file extensions (excluding
    the leading dot).}
  \item{all.files}{a logical.  If \code{FALSE} (default), only visible
    files are considered; if \code{TRUE}, all files are used.}
  \item{full.names}{a logical indicating whether the full paths of the
    files found are returned (default), or just the file names.}
  \item{type}{a character string giving the \sQuote{type} of the files
    to be listed, as characterized by their extensions.  Currently,
    possible values are \code{"code"} (R code), \code{"data"} (data
    sets), \code{"demo"} (demos), \code{"docs"} (R documentation), and
    \code{"vignette"} (vignettes).}
  \item{OS_subdirs}{a character vector with the names of OS-specific
    subdirectories to possibly include in the listing of R code and
    documentation files.  By default, the value of the environment
    variable \env{R_OSTYPE}, or if this is empty, the value of
    \code{\link{.Platform}$OS.type}, is used.}
}
\details{
  \code{file_ext} returns the file (name) extensions (excluding the
  leading dot).
  (Only purely alphanumeric extensions are recognized.)

  \code{file_path_as_absolute} turns a possibly relative file path
  absolute, performing tilde expansion if necessary.  This is a wrapper
  for \code{\link{normalizePath}}.  Currently, \code{x} must be a single
  existing path.

  \code{file_path_sans_ext} returns the file paths without extensions
  (and the leading dot).
  (Only purely alphanumeric extensions are recognized.)

  \code{list_files_with_exts} returns the paths or names of the files in
  directory \code{dir} with extension matching one of the elements of
  \code{exts}.  Note that by default, full paths are returned, and that
  only visible files are used.

  \code{list_files_with_type} returns the paths of the files in \code{dir}
  of the given \sQuote{type}, as determined by the extensions recognized
  by \R.  When listing R code and documentation files, files in
  OS-specific subdirectories are included if present according to the
  value of \code{OS_subdirs}.  Note that by default, full paths are
  returned, and that only visible files are used.
}
\seealso{
  \code{\link{file.path}},
  \code{\link{file.info}},
  \code{\link{list.files}}
}
\examples{\donttest{
dir <- file.path(R.home(), "library", "stats")
list_files_with_exts(file.path(dir, "demo"), "R")
list_files_with_type(file.path(dir, "demo"), "demo") # the same
file_path_sans_ext(list.files(file.path(R.home("modules"))))
}}
\keyword{file}
