% File src/library/utils/man/windows/shortPathName.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{shortPathName}
\alias{shortPathName}
\title{Express File Paths in Short Form}
\description{
  Convert file paths to the short form.  This is an interface to the
  Windows API call \code{GetShortPathName}.
}
\usage{
shortPathName(path)
}
\arguments{
  \item{path}{character vector of file paths.}
}
% http://msdn.microsoft.com/en-gb/library/windows/desktop/aa364989%28v=vs.85%29.aspx
\details{
  For most file systems, the short form is the \sQuote{DOS} form with
  8+3 path components and no spaces, and this used to be guaranteed.
  But some file systems on recent versions of Windows do not have short
  path names when the long-name path will be returned instead.
}
\value{
  A character vector.  The path separator will be \code{\\}.  If a
  file path does not exist, the supplied path will be returned with slashes
  replaced by backslashes.
}
\note{
  This is only available on Windows.
}
\seealso{
  \code{\link{normalizePath}}.
}
\examples{\donttest{
cat(shortPathName(c(R.home(), tempdir())), sep = "\n")
}
\testonly{
cat(shortPathName(R.home()), sep = "\n")
}}
\keyword{ utilities }
