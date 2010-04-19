% File src/library/base/man/Sys.which.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-10 R Core Development Team
% Distributed under GPL 2 or later

\name{Sys.which}
\alias{Sys.which}
\title{Find Full Paths of Executables}
\description{
  This is an interface to the system command \code{which}.
}
\usage{
Sys.which(names)
}
\arguments{
  \item{names}{Character vector of names of possible executables.}
}
\details{
  The system command \code{which} reports on the full names of an
  executable (including an executable script) found on the current path.

#ifdef windows
  On Windows an \sQuote{executable} is a file with extension
  \file{.exe}, \file{.com}, \file{.cmd} or \file{.bat}.  Such files need
  not actually be executable, but this is what \code{\link{system}}
  tries.
#endif
}
\value{
  A character vector of the same length as \code{names}, named by
  \code{names}.  The elements are either the full path to the
  executable/script or some indication that no executable of that name was
  found.  Typically the indication is \code{""}, but this does depend on
  the OS (and is a message on Solaris).

#ifdef windows
  The paths will be short paths (8+3 components, no spaces) with
  \code{\\} as the path delimiter.
#endif  
}
\examples{
## the first two are likely to exist everywhere
## texi2dvi exists on most Unix-alikes and under MiKTeX
Sys.which(c("ftp", "ping", "texi2dvi", "this-does-not-exist"))
}
\keyword{ utilities }
