% File src/library/utils/man/windows/winextras.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
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
  \code{\link{bug.report}} which returns a character string describing
  the version of Windows in use. 
}
\note{
This function is only available on Microsoft Windows.  
    
The result is based on the Windows
\code{GetVersionEx} API function which reports 
erroneous information in Windows 8.1 and later.
}
\keyword{utilities}
