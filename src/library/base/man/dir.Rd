\name{dir}
\title{List Directory/Folder Contents}
\usage{
dir(name)
}
\alias{name}
\arguments{
  \item{name}{a character string, specifying the directory or
    folder for which the contents are to be obtained.}
}
\description{
  \code{dir} returns a character vector containing a list of the
  files contained in the named directory or folder.}
}
\examples{
dir(R.home())   # Files at the top-level of the R system
}
\keyword{file}
\keyword{utilities}
