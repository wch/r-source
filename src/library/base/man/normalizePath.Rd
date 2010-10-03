% File src/library/utils/man/normalizePath.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{normalizePath}
\alias{normalizePath}
\title{Express File Paths in Canonical Form}
\description{
  Convert file paths to canonical form for the platform, to display them
  in a user-understandable form and so relative and absolute paths can
  be compared.
}
\usage{
normalizePath(path, winslash = "\\\\", mustWork = NA)
}
\arguments{
  \item{path}{character vector of file paths.}
  \item{winslash}{the path separator to be used on Windows -- ignored
    elsewhere.  Must be one of \code{c("/", "\\\\")}.}
  \item{mustWork}{logical: if \code{TRUE} then an error is given if the result
    cannot be determined; if \code{NA} then a warning.}
}
\details{  
  Where the Unix-alike platform supports it this turns paths into
  absolute paths in their canonical form (no \samp{./}, \samp{../} nor
  symbolic links).  Every attempt is made to ensure that the path
  returned is unique, using the \code{realpath} system call where available.

  On Windows it converts relative paths to absolute paths, and converts
  short names for path elements to long names.
}
\value{
  A character vector.

  If a path is not a real path the result is system-dependent (unless
  \code{mustWork = TRUE}, when this is an error).
  On Unix-alikes, this will likely be the corresponding input
  element.  On Windows, the result will be either the corresponding input
  element or a transformation of it into an absolute path.
}
\seealso{
  \code{\link{file_path_as_absolute}} in package \pkg{tools}.
#ifdef windows
  
  \code{\link{shortPathName}}
#endif
}
\examples{\donttest{ # random tempdir
cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
}}
\keyword{ utilities }
