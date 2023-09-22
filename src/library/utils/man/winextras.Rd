% File src/library/utils/man/winextras.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{winextras}
\alias{win.version}
\title{Get Windows Version}
\description{
  Get the self-reported Microsoft Windows version number.
}
\usage{
win.version()
}
\details{
  \code{win.version} is an auxiliary function for
  \code{\link{sessionInfo}} and \code{\link{bug.report}}.
}
\value{
  A character string describing the version of Windows reported to be in use.
}
\note{
  This function is only available on Microsoft Windows.

  The result is based on the Windows \code{GetVersionEx} API function.  It
  is not known how to detect a version of Windows before it is released, and
  hence the textual information returned by \R may identify an older version
  than installed.  The build number is more reliable.  When running \R in
  compatibility mode, the reported version including the build number is the
  compatibility version, not the installed version.
}
\examples{
if(.Platform$OS.type == "windows")
   print(win.version())
}
\keyword{utilities}
