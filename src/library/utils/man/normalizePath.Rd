\name{normalizePath}
\alias{normalizePath}
\title{Express File Paths in Canonical Form}
\description{
  Convert file paths to canonical form for the platform.
}
\usage{
normalizePath(path)
}
\arguments{
  \item{path}{character vector of file paths.}
}
\details{
  Currently this only does anything useful on Windows, where it converts
  relative paths to absolute paths, and on Windows 98, ME, 2000 or
  later converts short names to long names.
}
\value{
  A character vector.
}
\examples{
cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
}
\keyword{ utilities }
