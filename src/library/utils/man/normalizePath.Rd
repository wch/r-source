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
#ifdef unix
  Where the platform supports it this turns paths into absolute paths
  in their canonical form (no \code{./}, \code{../} nor symbolic links).
#endif
#ifdef windows
  This converts relative paths to absolute paths, and on Windows 98, ME,
  2000 or later converts short names to long names.  It will always use
  backlashes as the path separator.
#endif

  If the path is not a real path the result is undefined but will most
  likely be the corresponding input element.
}
\value{
  A character vector.
}
\examples{
cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
}
\keyword{ utilities }
