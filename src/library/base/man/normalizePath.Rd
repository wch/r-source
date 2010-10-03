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
  \item{winslash}{the separator to be used on Windows -- ignored
    elsewhere.  Must be one of \code{c("/", "\\\\")}.}
  \item{mustWork}{logical: if \code{TRUE} then an error is given if the result
    cannot be determined; if \code{NA} then a warning.}
}
\details{  
  Where the Unix-alike platform supports it attempts to turn paths into
  absolute paths in their canonical form (no \samp{./}, \samp{../} nor
  symbolic links).  It relies on the POSIX system function
  \code{realpath}: if the platform does not have that (we know of no
  current example) then the result will be an absolute path but might
  not be canonical.

  On Windows it converts relative paths to absolute paths, converts
  short names for path elements to long names and ensures the separator
  is that specified by \code{winslash}.
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
    \item The canonicalized path would be exceed the maximum length of a
    file path.
  }
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
