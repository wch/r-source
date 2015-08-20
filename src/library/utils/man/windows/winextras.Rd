% File src/library/utils/man/windows/winextras.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{winextras}
\alias{win.version}
\title{Get Windows Version}
\description{
  Get the self-reported Windows version number.  
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
    
  The result is based on the Windows \code{GetVersionEx} API function,
  which for recent versions of Windows reports the compatibility version,
  and not necessarily the actual version (hence 8.1 and 10 may be reported
  as 8). If the API call reports 8, this function returns
  \code{>= 8}.
}
\keyword{utilities}
