% File src/library/utils/man/DLL.version.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{DLL.version}
\alias{DLL.version}
\title{DLL Version Information on MS Windows}
\description{
  On MS Windows only, return the version of the package and the version of \R used to
  build the DLL, if available.
}
\usage{
DLL.version(path)
}
\arguments{
  \item{path}{character vector of length one giving the complete path to
    the DLL.}
}
\value{
  If the DLL does not exist, \code{NULL}.

  A character vector of length two, giving the DLL version and the version of
  \R used to build the DLL.  If the information is not available, the
  corresponding string is empty.
}
\note{
  This is only available on Windows.
}
\examples{
if(.Platform$OS.type == "windows") withAutoprint({
  DLL.version(file.path(R.home("bin"), "R.dll"))
  DLL.version(file.path(R.home(), "library/stats/libs", .Platform$r_arch, "stats.dll"))
})
}
\keyword{utilities}
