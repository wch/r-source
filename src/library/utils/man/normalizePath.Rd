% File src/library/utils/man/normalizePath.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{normalizePath}
\alias{normalizePath}
\title{Express File Paths in Canonical Form}
\description{
  Convert file paths to canonical form for the platform, to display them
  in a user-understandable form.
}
\usage{
normalizePath(path)
}
\arguments{
  \item{path}{character vector of file paths.}
}
\details{  
  Where the Unix-alike platform supports it this turns paths into
  absolute paths in their canonical form (no \samp{./}, \samp{../} nor
  symbolic links).

  On Windows it converts relative paths to absolute paths, and converts
  short names to long names.  It will always use backslashes as the path
  separator.
}
\value{
  A character vector.

  If a path is not a real path the result is undefined.  On 
  Unix-alikes, this will likely be the corresponding input element.
  On Windows, a warning will be issued, and the result will be either
  the corresponding input element, or a transformation of it
  into an absolute path.
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
