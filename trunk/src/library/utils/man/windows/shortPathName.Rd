\name{shortPathName}
\alias{shortPathName}
\title{Express File Paths in Short Form}
\description{
  Convert file paths to the short (DOS) form, with 8+3 path components
  and no spaces.  This is an interface to the Windows API call
  \code{GetShortPathName}.
}
\usage{
shortPathName(path)
}
\arguments{
  \item{path}{character vector of file paths.}
}
\value{
  A character vector.  The path separator will be \code{\\}.
}
\seealso{
  \code{\link{normalizePath}}.
}
\examples{
cat(shortPathName(c(R.home(), tempdir())), sep = "\n")
}
\keyword{ utilities }
