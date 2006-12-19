\name{Sys.glob}
\alias{Sys.glob}
\title{Wildcard Expansion on File Paths}
\description{
  Function to do wildcard expansion (also known as \sQuote{globbing}) on
  file paths.

#ifdef unix
  Not all platforms support this.
#endif
#ifdef windows
  Not available on Windows.
#endif
}
\usage{
Sys.glob(paths)
}
\arguments{
  \item{paths}{character vector of relative or absolute filepaths.}
}
\value{
  A character vector of matched file paths.

  An error, on platforms which do not have the \code{glob} system call.
}
\seealso{
  \code{\link{path.expand}}
}
\examples{
\dontrun{
Sys.glob(file.path(R.home(), "library", "*", "R", "*.rdx"))
}}
\keyword{utilities}
\keyword{file}
